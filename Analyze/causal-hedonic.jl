## Simulate Bayesian hedonic selection model

## --------------------------------------------------------------------------- #
## load packages
##using Gadfly

## --------------------------------------------------------------------------- #

## DGP parameters

## rng seed
my_seed = 2099

## sample size
n = 1000

## selection equation
gamma = [-1.0, 1.0, 1.0]
##gamma = [-1, 1, 1, 1] # with factor f

## treatment outcome
beta_1 = [2.0, 10.0]
##beta.1 = [2, 10, 1]

## control outcome
beta_0 = [1.0, 2.0]
##beta.0 = [1, 2, 1]

beta_true = vcat(gamma, beta_1, beta_0)

Sigma_true = [1.0 0.7 -0.7;
              0.7 1 -0.1;
              -0.7 -0.1 1.0]

## --------------------------------------------------------------------------- #

## Generate the data


## --------------------------------------------------------------------------- #

## setting the RNG

Random.seed!(my_seed)

## --------------------------------------------------------------------------- #

## predictors

x = rand(n) # predictor

z = rand(n) # instrument

f = StatsBase.sample(["a","b"], n) # factor

ydummy = rand(0:1, n)

## full data frame
df = DataFrames.DataFrame()
df[:X] = x;
df[:Z] = z;
df[:F] = f;
df[:Y_dummy] = ydummy;
DataFrames.categorical!(df, [:F, :Y_dummy]);

## errors
errs = rand( Distributions.MvNormal(Sigma_true), n )

errs_df = DataFrames.DataFrame()
errs_df[:v] = errs[1,:];
errs_df[:u1] =errs[2,:];
errs_df[:u0] = errs[3,:];

## selection equation
## TODO: add intercept?? appears to include by default...
formula_d = StatsModels.@formula(Y_dummy ~ X + Z)

## with factor:
## formula_d = (Y_dummy ~ (X+F+Z))

mm_d = StatsModels.ModelMatrix( StatsModels.ModelFrame(formula_d, df) )

d_star = mm_d.m*gamma + errs_df[:v]

## potential outcomes
formula_y = StatsModels.@formula(Y_dummy ~ X)
##formula_y = (Y_dummy ~ X)

## with factor:
##formula_y = (Y_dummy ~ X + F)

mm_y = StatsModels.ModelMatrix( StatsModels.ModelFrame(formula_y, df))

y_1 = mm_y.m*beta_1 + errs_df[:u1]

y_0 = mm_y.m*beta_0 + errs_df[:u0]

## d_obs = ifelse( d_star > 0, 1, 0 )
d_obs = [ di > 0 ? 1 : 0 for di in d_star ]

y_obs = d_obs .* y_1 + (1 .- d_obs) .* y_0

## gather the data
df[:Y_obs] = y_obs;
df[:D_obs] = d_obs;

xdf = df[ [:Y_obs, :X, :Z, :F]];
zdf = df[ [:D_obs, :X, :Z, :F]];

## true treatment effects
ate_true = y_1 .- y_0

##Gadfly.plot(x=ate_true, Geom.density)
Statistics.mean(ate_true)

(tt_true = sum(d_obs .* (y_1 - y_0))/sum(d_obs))

(tut_true = sum((1 .- d_obs) .* (y_1 - y_0)) / sum(1 .- d_obs))

## --------------------------------------------------------------------------- #

## Gibbs sampler

## gather variable names to construct formula
## TODO: defining formula_rhs unnecessary...
##       are x_names, z_names necessary?

x_names = setdiff(names(xdf), [:Y_obs, :Y_dummy, :D_obs, :Z])
z_names = setdiff(names(zdf), [:Y_obs, :Y_dummy, :D_obs])
z_names = setdiff(z_names, x_names)

global formula_rhs = ""
for i in x_names
    global formula_rhs = join([formula_rhs; string(i)], " + " )
end
global formula_rhs = replace(formula_rhs, r"^\s\+\s" => "" )

##formula_y = DataFrames.Formula(:Y_obs, parse(formula_rhs) )
formula_y = StatsModels.@formula(Y_obs ~ X)

global formula_rhs = join([formula_rhs; z_names], " + " )

##formula_d = DataFrames.Formula(:D_obs, parse(formula_rhs) )
formula_d = StatsModels.@formula(D_obs ~ X + Z)

ktot = length( beta_true )

## --------------------------------------------------------------------------- #

my, sy = Statistics.mean(y_obs), Statistics.std(y_obs)

println("Toy model loaded!")

## --------------------------------------------------------------------------- #
