## Fit (default: DP Mixture) hedonic selection model
##nohup nice julia --precompiled=yes fit_dpm.jl "fmn" 5000 2 &> fmn.log &

## --------------------------------------------------------------------------- #

## load causal mixtures module
if !in( "CausalMixtures", readdir() )
    error("Module not found!")
end

push!(LOAD_PATH, ".")
using CausalMixtures # loads JLD, HDF5

## --------------------------------------------------------------------------- #

if length(ARGS) == 0
    ## set defaults:
    model = "dpm" # options: {dpm, blocked, fmn, gaussian}
    M = "2500"
    J = "20"
else
    model = ARGS[1]
    M = ARGS[2]
    if length(ARGS) == 3
        J = ARGS[3]
    end
end

compress_out = true

@printf("Preparing to fit %s model, for %s iterations...\n", model, M)

## --------------------------------------------------------------------------- #
## set paths

path_to_raw = "./Data"
name_of_df = "df.jld"

path_to_output = joinpath("/home/juanfung/Documents/scpd/Analyze/Data/", model)

if !isdir(path_to_output)
    mkpath(path_to_output)
end

## --------------------------------------------------------------------------- #
## seed random number generator

Random.seed!(12981)

## --------------------------------------------------------------------------- #
## init JIT
@printf("Initializing JLD...\n")
if !isdir("./tmp") mkpath("./tmp") end
@time JLD.jldopen("./tmp/test.jld", "w", compress=true) do file
    write(file, "M", M)
end;

## --------------------------------------------------------------------------- #
## load pre-processed DataFrame and model Formulas

@time dfs = JLD.jldopen( joinpath(path_to_raw, name_of_df), "r+");

df = read(dfs, "df");
y_form = read(dfs, "y_form");
d_form = read(dfs, "d_form");
dims = dfs["dims"]
kx = read(dims, "kx");
kz = read(dims, "kz");
ktot = read(dims, "ktot");

## --------------------------------------------------------------------------- #
## prior settings

if model == "gaussian"
    J = 1
else
    J = parse(Int, J)
end

alpha = 1.0
alpha_shape = 0.0
alpha_rate = 0.0

beta_mu = zeros(ktot);
beta_nu = 100
beta_V = beta_nu*eye(ktot);

rho = 5
r = 1
R = diagm([1/rho, r, r])

## --------------------------------------------------------------------------- #
## parameter settings

M = parse(Int, M)

## --------------------------------------------------------------------------- #
## initialize sampler

## collect raw data
data_raw = CausalMixtures.RawData(y_form, d_form, df);

## collect priors
prior_dp = CausalMixtures.PriorDP(alpha=alpha, J=J, alpha_shape=alpha_shape, alpha_rate=alpha_rate);
prior_beta = CausalMixtures.PriorBeta(mu=beta_mu, V=beta_V\eye(ktot), Vinv=true);
prior_sigma = CausalMixtures.PriorSigma(rho=rho, R=R);
prior_theta = CausalMixtures.PriorTheta(prior_beta=prior_beta, prior_Sigma=prior_sigma);
prior_beta = prior_sigma = 0
prior = CausalMixtures.InputPriors(prior_dp=prior_dp, prior_theta=prior_theta);
prior_dp = prior_theta = 0

## collect params
param = CausalMixtures.InputParams(M=1, scale_data=(true,true), verbose=true, model=model);

## --------------------------------------------------------------------------- #
## init JIT

## initialize sampler
@time out = CausalMixtures.dpm_init(data_raw, prior, param);

## clear workspace
data_raw = prior = param = df = facs = xvars = zvars = y_obs = d_obs = mm_d = mm_y = beta_mu = beta_V = 0;

## run 1 iteration
@time out = CausalMixtures.dpm!(out...);

## --------------------------------------------------------------------------- #
## continue chain

## set M iterations
out[2].params.M = M
@time out = CausalMixtures.dpm!(out...);

## --------------------------------------------------------------------------- #
## save state, input, output
println("Saving state and input to disk...")

@printf("Step 5: saving state and input...\n")
@time JLD.jldopen( joinpath(path_to_output, "state.jld"), "w", compress=compress_out) do file
    JLD.addrequire(file, CausalMixtures)
    write(file, "state", out[1]) # save state
    write(file, "input", out[2]) # save inputs
end;

println("Saving output to disk...")

fname = "output/out-1-$(out[1].state_sampler.batch_m).jld"

@time JLD.jldopen( joinpath(path_to_output, fname), "w", compress=compress_out) do file
    JLD.addrequire(file, CausalMixtures)
    write(file, "out", out[3]) # save output
end;

@printf("First M = %d iterations complete and saved!", out[1].state_sampler.batch_m)

## --------------------------------------------------------------------------- #
