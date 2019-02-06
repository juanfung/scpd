## test samplers
## nohup nice julia --precompiled=yes causal-sims.jl "blocked" 10000 20 &> test-blocked.log &
## nohup nice julia --precompiled=yes causal-sims.jl "fmn" 10000 20 &> test-fmn.log &

## --------------------------------------------------------------------------- #

## load module (packages, functions, etc.)
##include("./CausalMixtures/src/CausalMixtures.jl")
push!(LOAD_PATH, ".")
using CausalMixtures
using DelimitedFiles, LinearAlgebra, Statistics, Printf, Gadfly 

## load test data
include("causal-hedonic.jl")

## --------------------------------------------------------------------------- #

if length(ARGS) == 0
    ## set defaults:
    model = "dpm" # options: {dpm, blocked, fmn, gaussian}
    M = "5000"
    J = "20"
else
    model = ARGS[1]
    M = ARGS[2]
    if length(ARGS) == 3
        J = ARGS[3]
    end    
end

Printf.@printf("Preparing to fit %s model, for %s iterations...\n", model, M)

path_to_data = "./tmp/test-" * model

if !isdir(joinpath(path_to_data, "output"))
    mkpath(joinpath(path_to_data, "output"))
end

compress_out = true

## --------------------------------------------------------------------------- #
## prior settings:

if model == "gaussian"
    J = 1
else
    J = parse(Int, J)
end

alpha = 1.0
alpha_shape = 0.0
alpha_rate = 0.0

beta_mu = zeros(ktot)
beta_nu = 100 # 10, 20, 50, 100
beta_V = beta_nu * Matrix(1.0I, ktot, ktot)

rho = 6 # 10, 20, 50, 100
r = 2 
R = Matrix(Diagonal([1/rho, r, r]))

## expected value of Sigma
( ESigma = (inv(rho*R)) / (rho-3-1) )

## parameter settings
##M = 5000
M = parse(Int, M)

println("Fitting " * model * ":\nnu = $(beta_nu)\nrho = $(rho)\nr = $(r)...")

## --------------------------------------------------------------------------- #
## collect inputs to init

## collect priors
prior_dp = CausalMixtures.PriorDP(alpha=alpha, J=J, alpha_shape=alpha_shape, alpha_rate=alpha_rate)
prior_beta = CausalMixtures.PriorBeta(mu=beta_mu, V=inv(beta_V), Vinv=true)
prior_sigma = CausalMixtures.PriorSigma(rho=rho, R=R)
prior_theta = CausalMixtures.PriorTheta(prior_beta=prior_beta, prior_Sigma=prior_sigma)
prior_in = CausalMixtures.InputPriors(prior_dp=prior_dp, prior_theta=prior_theta)
## collect params
param_dpm = CausalMixtures.InputParams(M=1, scale_data=(true,true), verbose=true, model=model)
## collect raw data
data_raw = CausalMixtures.RawData(formula_y, formula_d, df)

## --------------------------------------------------------------------------- #
## initialize sampler
@printf("Step 1: Initializing state and input...\n")
@time jit_init = CausalMixtures.dpm_init(data_raw, prior_in, param_dpm)

@printf("Step 2: Single run...\n")
@time out = CausalMixtures.dpm!(jit_init...)

@printf("Step 3: Continue run...\n")
out[2].params.M = M
out[2].params.verbose = false
@time out = CausalMixtures.dpm!(out...)
##@time (jit_state, jit_input, out) = dpm_init(data_raw, prior_dpm, param_dpm)
##state.state_dp.njs = StatsBase.countmap(collect(values(state.state_dp.labels))

@printf("Step 5: saving state and input...\n")
@time JLD.jldopen( joinpath(path_to_data, "state.jld"), "w", compress=compress_out) do file
    JLD.addrequire(file, CausalMixtures)
    write(file, "state", out[1])
    write(file, "input", out[2])
end;

@printf("Step 6: saving output...\n")
fname = "out-1-$(out[1].state_sampler.batch_m)"
@time JLD.jldopen(joinpath(path_to_data, "output/" * fname * ".jld"), "w", compress=compress_out) do file
    JLD.addrequire(file, CausalMixtures)
    write(file, "out", out[3])
end;

##@time JLD.jldopen(path_to_data * "out.jld", "w", compress=compress_out) do file
##    JLD.addrequire(file, CausalMixtures)
##    write(file, "state", out[1])
##    write(file, "input", out[2])
##    g = HDF5.g_create(file, "output")    
##    g["sims"] = out[3]
##end;

@printf("First M = %d iterations complete and saved!\n", out[1].state_sampler.batch_m)
@printf("Test sims: done!")

## --------------------------------------------------------------------------- #

## TODO:
## CHECK ASSIGNMENTS BY REFERENCE VS COPIES FOR OBJECTS THAT SHOULD NOT MUTATE!!!

## --------------------------------------------------------------------------- #

## generate znew:
println("Loading znew...")

(state, input, output) = out;

znew = vcat(mean(input.data.Hmat[1:1000,1:3], dims=1),
            median(Array(input.data.Hmat[1:1000,1:3]), dims=1))'

writedlm("./tmp/test/zvec2016test.dat", znew)

# load znew
# znew = readdlm("./tmp/test/zvec2016test.dat", ',', Float64);

## Get PPD draws and compute treatment effects
@time ynew = CausalMixtures.rand_ppd(output, input, znew[:,1]);

@time tes = CausalMixtures.dpm_ate(ynew, input);

## compute summary stats    
StatsBase.summarystats(tes.ate)

StatsBase.summarystats(tes.tt)



##pdfs = Gadfly.plot( layer(x=tes.ate, Geom.density, Theme(default_color=colorant"red")),
##                    layer(x=tes.tt, Geom.density, Theme(default_color=colorant"blue")),
##                    Guide.manual_color_key("Legend", ["ATE", "TT"], ["red", "blue"]))
