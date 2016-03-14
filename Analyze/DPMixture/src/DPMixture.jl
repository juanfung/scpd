## Module for DPM selection model

module DPMixture

## load packages
using DataFrames, Distributions, Debug, JLD

## --------------------------------------------------------------------------- #
## Priors

abstract InputType
## TODO: convert to AbstractArray types

## concentration parameter prior
immutable PriorDP <: InputType
    J::Int64
    alpha::Float64
    alpha_shape::Float64
    alpha_rate::Float64
end

## default contructor
PriorDP(; J=2, alpha=1.0, alpha_shape=0.0, alpha_rate=0.0) =
    PriorDP(J, alpha, alpha_shape, alpha_rate)

## coefficient prior
immutable PriorBeta <: InputType
    beta_mu::Array{Float64}
    beta_nu::Int64
    beta_V::Array{Float64}
end

## default constructor
PriorBeta(; beta_mu=zeros(2), beta_nu=10, beta_V=eye(2)) = PriorBeta(beta_mu, beta_nu, beta_V)

## cov prior
immutable PriorSigma <: InputType
    sigma_rho::Int64
    sigma_R::Array{Float64,2}
end

## default contructor
PriorSigma(; sigma_rho=5, sigma_R=eye(3)) = PriorSigma(sigma_rho, sigma_R)    

## collect prior settings
immutable PriorTuple
    prior_dp::PriorDP
    prior_beta::PriorBeta
    prior_sigma::PriorSigma
end

## default constructor
PriorTuple(; prior_dp=PriorDP(), prior_beta=PriorBeta(), prior_sigma=PriorSigma() ) =
    PriorTuple(prior_dp, prior_beta, prior_sigma)

## --------------------------------------------------------------------------- #
## Sampler parameters

type ParamTuple <: InputType
    M::Int64
    scale_data::Tuple{Bool,Bool}
    verbose::Bool
end

## default constructor
ParamTuple(; M=100, scale_data=(false,false), verbose=true) = ParamTuple(M, scale_data, verbose)

## --------------------------------------------------------------------------- #
## Data

## data input
immutable DataTuple <: InputType
    y_form::Formula
    d_form::Formula
    df::DataFrame
end

## standardized data
immutable ScaleData <: InputType
    a::Array{Float64}
    m::Array{Float64,1}
    s::Array{Float64,1}
end

ScaleData(; a=[0.0], m=[0.0], s=[1.0]) = ScaleData(a, m, s)

## data matrices used by sampler
immutable GibbsData <: InputType
    y::Union{Array{Float64,1},ScaleData}
    d::Array{Int64,1}
    d_l::Array{Float64,1}
    d_u::Array{Float64,1}
    xmat::Union{Array{Float64,2},ScaleData}
    zmat::Union{Array{Float64,2},ScaleData}
    Hmat::SparseMatrixCSC{Float64,Int64}
end

## default constructor
GibbsData(; y=[0.0], d=[0], d_l=[0.0], d_u=[0.0], xmat=[0.0], zmat=[0.0], Hmat=sparse([0.0])) =
    GibbsData(y, d, d_l, d_u, xmat, zmat, Hmat)

## --------------------------------------------------------------------------- #
## Current state

abstract StateType

## current state for augmented data
type StateData <: StateType
    dstar::Array{Float64,1}
    ymiss::Array{Float64,1}
    y1::Array{Float64,1}
    y0::Array{Float64,1}
    yuse::Array{Float64,2}
end

## default constructor
StateData(; dstar=[0.0], ymiss=[0.0], y1=[0.0], y0=[0.0], yuse=[0.0]) =
    StateData(dstar, ymiss, y1, y0, yuse)

## current state for DP parameters
type StateDP <: StateType
    J::Int64
    alpha::Float64
    label::Array{Int64,1}
    eta::Float64
end

## default constructor
StateDP(; J=2, alpha=1.0, label=[0], eta=0.0) = StateDP(J, alpha, label, eta)

## current state for component parameters
type StateTheta <: StateType
    betas::Array{Float64}
    Sigma::Array{Float64}
end

## default constructor
StateTheta(; betas=zeros(2), Sigma=eye(3)) = StateTheta(betas, Sigma)

type StateSampler
    chain::Bool
    batch_n::Int64
    batch_m::Int64
    ##rng::AbstractRNG
end

StateSampler(; chain=false, batch_n=1, batch_m=0) = StateSampler(chain, batch_n, batch_m)

## collect current state
type GibbsState
    state_data::StateData
    state_dp::StateDP
    state_theta::StateTheta    
    chain::Bool
    batch_n::Int64
    batch_m::Int64
end

## default constructor
GibbsState(; state_data=StateData(), state_dp=StateDP(), state_theta=StateTheta(),
           chain=false, batch_n=1, batch_m=0) = GibbsState(state_data, state_dp, state_theta,
                                                           chain, batch_n, batch_m)

## --------------------------------------------------------------------------- #
## Parameters and constants

## collect dimension
immutable DimTuple <: InputType
    n::Int64
    kx::Int64
    kz::Int64
    ktot::Int64
end

## default constructor
DimTuple(; n=0, kx=0, kz=0, ktot=0) = DimTuple(n, kx, kz, ktot)

immutable GibbsConstant
    dim::DimTuple
    prior::PriorTuple
    param::ParamTuple
end

GibbsConstant(; dim=DimTuple(), prior=PriorTuple(), param=ParamTuple()) =
    GibbsConstant(dim, prior, param)

## --------------------------------------------------------------------------- #
## Pre-allocation

abstract OutputType

## augmented data, output
type OutData <: OutputType
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
type OutDP <: OutputType
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
type OutTheta <: OutputType
    betas_out::Array{Array{Float64,2},1}    
    Sigma_out::Union{Array{Array{Float64,2},1},Array{Array{Float64,3},1}}
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
export InputType, OutputType, StateType
export PriorDP, PriorBeta, PriorSigma, PriorTuple, ParamTuple, DataTuple
export StateData, StateDP, StateTheta
export OutData, OutDP, OutTheta, OutTuple
export GibbsData, GibbsState, GibbsConstant, GibbsInit, GibbsOut
export PosteriorPredictive, PPD
export ScaleData

## export functions
export flip_mat, NobileWishart, truncnorm, standardize, rescale_output, scale_input, rescale_beta
export dpmixture_init, dpmixture_chain, dpmixture_gibbs, dpmixture_blocked, dpmixture_dump, dpmixture
export dpmixture_ppd
export dpmixture_fgibbs, sample_alpha, remove_component, shift_label, sample_label
export fmn_gibbs # FMN model
export gaussian_gibbs # Gaussian model

## --------------------------------------------------------------------------- #
## load misc functions
include("misc_functions.jl")
include("gibbs_functions.jl")

## initialization, chaining functions
include("dpmixture_init.jl")

## marginalized polya urn sampler
include("dpmixture_gibbs.jl")
include("dpmixture_fgibbs.jl")

## blocked gibbs sampler
include("dpmixture_blocked.jl")

## fmn gibbs sampler
include("fmn_gibbs.jl")

## benchmark guassian gibbs sampler
include("gaussian_gibbs.jl")

## posterior predictives
include("dpmixture_ppd.jl")

end
