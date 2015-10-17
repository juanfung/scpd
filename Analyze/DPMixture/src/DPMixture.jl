## Module for DPM selection model

module DPMixture

## load packages
using DataFrames, Distributions, Debug

## --------------------------------------------------------------------------- #
## Priors

## concentration parameter prior
type PriorDP
    J::Int64
    alpha::Float64
    alpha_shape::Float64
    alpha_rate::Float64
end

## default contructor
PriorDP(; J=2, alpha=1.0, alpha_shape=0.0, alpha_rate=0.0) =
    PriorDP(J, alpha, alpha_shape, alpha_rate)

## coefficient prior
type PriorBeta
    beta_mu::Array{Float64}
    beta_nu::Int64
    beta_V::Array{Float64}
end

## default constructor
PriorBeta(; beta_mu=zeros(2), beta_nu=10, beta_V=eye(2)) = PriorBeta(beta_mu, beta_nu, beta_V)

## cov prior
type PriorSigma
    sigma_rho::Int64
    sigma_R::Array{Float64}    
end

## default contructor
PriorSigma(; sigma_rho=5, sigma_R=eye(3)) = PriorSigma(sigma_rho, sigma_R)    

## collect prior settings
type PriorTuple
    prior_dp::PriorDP
    prior_beta::PriorBeta
    prior_sigma::PriorSigma
end

## default constructor
PriorTuple(; prior_dp=PriorDP(), prior_beta=PriorBeta(), prior_sigma=PriorSigma() ) =
    PriorTuple(prior_dp, prior_beta, prior_sigma)

## Sampler parameters
type ParamTuple
    M::Int64
    scale_data::Bool
    verbose::Bool
end

## default constructor
ParamTuple(; M=100, scale_data::Bool=false, verbose::Bool=true) = ParamTuple(M, scale_data, verbose)

## --------------------------------------------------------------------------- #
## Data

## data input
type DataTuple
    y_form::Formula
    d_form::Formula
    df::DataFrame
end

## data matrices used by sampler
type GibbsData
    y::Array{Float64}
    d::Array{Int64}
    d_l::Array{Float64}
    d_u::Array{Float64}
    xmat::Array{Float64}
    zmat::Array{Float64}
    Hmat::SparseMatrixCSC{Float64,Int64}
end

## default constructor
GibbsData(; y=[0.0], d=[0], d_l=[0.0], d_u=[0.0], xmat=[0.0], zmat=[0.0], Hmat=sparse([0.0])) =
    GibbsData(y, d, d_l, d_u, xmat, zmat, Hmat)

## --------------------------------------------------------------------------- #
## Current state

## current state for augmented data
type StateData
    dstar::Array{Float64}
    ymiss::Array{Float64}
    y1::Array{Float64}
    y0::Array{Float64}
    yuse::Array{Float64}
end

## default constructor
StateData(; dstar=[0.0], ymiss=[0.0], y1=[0.0], y0=[0.0], yuse=[0.0]) =
    StateData(dstar, ymiss, y1, y0, yuse)

## current state for DP parameters
type StateDP
    J::Int64
    alpha::Float64
    label::Array{Int64}
    eta::Float64
end

## default constructor
StateDP(; J=2, alpha=1.0, label=[0], eta=0.0) = StateDP(J, alpha, label, eta)

## current state for component parameters
type StateTheta
    betas::Array{Float64}
    Sigma::Array{Float64}
end

## default constructor
StateTheta(; betas=zeros(2), Sigma=eye(3)) = StateTheta(betas, Sigma)

## collect current state
type GibbsState
    state_data::StateData
    state_dp::StateDP
    state_theta::StateTheta
    chain::Bool
end

## default constructor
GibbsState(; state_data=StateData(), state_dp=StateDP(), state_theta=StateTheta(), chain=false) =
    GibbsState(state_data, state_dp, state_theta, chain)

## --------------------------------------------------------------------------- #
## Collect constants

type DimTuple
    n::Int64
    kx::Int64
    kz::Int64
    ktot::Int64
end

## default constructor
DimTuple(; n=0, kx=0, kz=0, ktot=0) = DimTuple(n, kx, kz, ktot)

type GibbsConstant
    dim::DimTuple
    prior::PriorTuple
    param::ParamTuple
end

GibbsConstant(; dim=DimTuple(), prior=PriorTuple(), param=ParamTuple()) =
    GibbsConstant(dim, prior, param)

## --------------------------------------------------------------------------- #
## Pre-allocation

## augmented data, output
type OutData
    dstar_out::Array{Float64}
    ymiss_out::Array{Float64}    
    y1_out::Array{Float64}
    y0_out::Array{Float64}
    y_out::Array{Float64}
end

## default constructor
OutData(; dstar_out=[0.0], ymiss_out=[0.0], y1_out=[0.0], y0_out=[0.0], y_out=[0.0]) =
    OutData(dstar_out, ymiss_out, y1_out, y0_out, y_out)

## DP parameters
type OutDP
    J_out::Array{Int64}
    label_out::Array{Int64}
    alpha_out::Array{Float64}
    eta_out::Array{Float64}
end

## default constructor
OutDP(; J_out=Array(Int64,0),
      label_out=Array(Int64,0),
      alpha_out=Array(Float64,0),
      eta_out=Array(Float64,0)) = OutDP(J_out, label_out, alpha_out, eta_out)

## component parameters (variable size)
type
    OutTheta
    betas_out::Array{Array{Float64,2},1} #Array{Any,1}
    Sigma_out::Array{Array{Float64,3},1}
end

## default constructor
OutTheta(; betas_out=Array(Array{Float64,2},0), Sigma_out=Array(Array{Float64,2},0)) =
    OutTheta(betas_out, Sigma_out)

## collect pre-allocated objects
type OutTuple
    out_M::Int64
    out_data::OutData
    out_dp::OutDP
    out_theta::OutTheta
end

## default constructor
OutTuple(; out_M=0, out_data=OutData(), out_dp=OutDP(), out_theta=OutTheta()) =
    OutTuple(out_M, out_data, out_dp, out_theta)

## --------------------------------------------------------------------------- #
## create abstract type for init, out types

abstract GibbsType

## --------------------------------------------------------------------------- #
## Sampler init

type GibbsInit <: GibbsType
    data_init::GibbsData
    constant_init::GibbsConstant
    state_init::GibbsState
end

## default contructor
GibbsInit(; data_init=GibbsData(), constant_init=GibbsConstant(), state_init=GibbsState()) =
    GibbsInit(data_init, constant_init, state_init)

## --------------------------------------------------------------------------- #
## Sampler output

type GibbsOut <: GibbsType
    out_tuple::OutTuple
    gibbs_init::GibbsInit
end

## default constructor
GibbsOut(; out_tuple=OutTuple(), gibbs_init=GibbsInit()) = GibbsOut(out_tuple, gibbs_init)

## --------------------------------------------------------------------------- #
## export to Main

## export types
export PriorDP, PriorBeta, PriorSigma, PriorTuple, ParamTuple, DataTuple
export StateData, StateDP, StateTheta
export OutData, OutDP, OutTheta, OutTuple
export GibbsData, GibbsState, GibbsConstant, GibbsInit, GibbsOut

## export functions
export flip_mat, NobileWishart, truncnorm
export dpmixture_init, dpmixture_chain, dpmixture_gibbs, dpmixture_blocked, mixture_gibbs, dpmixture

## --------------------------------------------------------------------------- #
## load functions

include("misc_functions.jl")
include("dpmixture_init.jl")
include("dpmixture_gibbs.jl")
include("dpmixture_blocked.jl")
include("mixture_gibbs.jl")

end
