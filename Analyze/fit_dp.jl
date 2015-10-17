## Fit DP Mixture selection model

## --------------------------------------------------------------------------- #

## load packages
using JLD

## load DP mixture module (packages, functions, etc.)
include("DPMixture.jl")

## --------------------------------------------------------------------------- #

## REMARKS:
## 1a. data should be limited to 1996-2001 (or mid 2002)... (decrease n)
## 1b. drop very low and very high Sale.Price
## 2. add dummies (or school year) dummies, tract dummies... (increase k)
## 3. revisit xvars, zvars...
## 4. learning alpha? more diffuse prior?
## 5. hyperpriors on theta?

## --------------------------------------------------------------------------- #

## Parallelize across workers:
## 1. wrap this file as
##function run(i::Int64)
##    df_path = "./Data/df_"*string(i)*".csv"
##    ...
##end
## 2. Then:
## out = mmap(run, [1:10])

## --------------------------------------------------------------------------- #

df = DataFrames.readtable("./Data/df_1.csv")

## pool factors
facs = [:Garage, :Agency_Name, :Post_Choice, :ProxB, :Buyer_Origin, :Year, :TRACTCE00]
DataFrames.pool!(df, facs)

## get outcome variables
xvars = DataFrames.readtable("./Data/xvars.csv")
xvars = Array(xvars[:x_1])
xvars = map(s -> replace(s, ".", "_"), xvars)

## get selection variables
zvars = readdlm("./Data/zvars.csv", ',', AbstractString)
zvars = zvars[2:end,2]
zvars = map(s -> replace(s, ".", "_"), zvars)

## get treatment choice
d_obs = df[:d_obs];

## get observed outcome
y_obs = df[:Log_Sale_Price];

## get models
form_rhs = ""
for i in 1:size(xvars, 1)
    form_rhs = join([form_rhs; xvars[i]], " + " )
end
form_rhs = replace(form_rhs, r"^\s\+\s", "" )

y_form = DataFrames.Formula(:Log_Sale_Price, parse(form_rhs) )

form_rhs = join([form_rhs; zvars], " + " )

d_form = DataFrames.Formula(:d_obs, parse(form_rhs) )

##mm_d = DataFrames.ModelFrame(d_form, df)
##mm_d = DataFrames.ModelMatrix( mm_d )
mm_d = DataFrames.ModelMatrix( DataFrames.ModelFrame(d_form, df) ).m;

mm_y = DataFrames.ModelMatrix( DataFrames.ModelFrame(y_form, df) ).m;

## --------------------------------------------------------------------------- #
## prior settings

kx = size(mm_y, 2)
kz = size(mm_d, 2)
ktot = 2kx + kz

beta_mu = zeros(ktot);
beta_nu = 10
beta_V = beta_nu*eye(ktot);

rho = 10
R = eye(3)

## parameter settings
M = 1000

prior_dp = DPMixture.PriorDP(alpha=1, J=20);
prior_beta = DPMixture.PriorBeta(beta_mu=beta_mu, beta_V=beta_V, beta_nu=beta_nu);
prior_sigma = DPMixture.PriorSigma(sigma_rho=rho, sigma_R=R);

prior = DPMixture.PriorTuple(prior_dp=prior_dp, prior_beta=prior_beta, prior_sigma=prior_sigma);

param = DPMixture.ParamTuple(M=M, scale_data=true)

## data input
data = DPMixture.DataTuple(y_form, d_form, df);

## --------------------------------------------------------------------------- #
## init JIT
@time init_jit = DPMixture.dpmixture_init(data, prior, DPMixture.ParamTuple(M=1, scale_data=true));
@time jit_out = DPMixture.dpmixture(init_jit);

## --------------------------------------------------------------------------- #
## init chain
@time init = DPMixture.dpmixture_init(data, prior, param);

## --------------------------------------------------------------------------- #
## run chain
@time gibbs_out = DPMixture.dpmixture(init);

## --------------------------------------------------------------------------- #

## save output
JLD.jldopen("out.jld", "w") do file
    addrequire(file, DPMixture)
    ##write(file, "init", init, "gibbs_out", gibbs_out)
    write(file, "gibbs_out", gibbs_out)
end

@printf("First M = %d iterations complete and saved!", M)

## --------------------------------------------------------------------------- #

## can later reload and continue chain:

## 1. reload:
##init = load("out.jld", "init")
##gibbs_out = load("./out.jld", "gibbs_out")

## 2. continue chain:
##@time gibbs_out = DPMixture.dpmixture(gibbs_out);

