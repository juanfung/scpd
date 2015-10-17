## Set up to run Gibbs sampler for DPM

## new chain:
## a. function(data,prior,params)
## b. call init(gibbs)
## b. call gibbs(init)

## continue chain:
## a. function(chain)
## b. call continue(chain)
## c. call gibbs(continue)

##init = function() end

## Inputs:
## Dat (data type object)
## Prior (prior type object)
## Param (param type object)

## Output:
## Gibbs (gibbs type object)

##using DataFrames, Distibutions

function dpmixture_init(data::DataTuple, prior::PriorTuple, param::ParamTuple)

    ## 1. get MCMC parameters

    M = param.M    
    scale_data = param.scale_data
    verbose = param.verbose
    if verbose
        println("M = ", M)
        println("scale data = ", scale_data)
        println("verbose = ", verbose)
    end   
    
    ## 2. get data -> model matrix
    ##    - optional: scale data
    
    y_form = data.y_form
    d_form = data.d_form
    df = data.df
    y = convert(Array, df[y_form.lhs])
    d = convert(Array, df[d_form.lhs])
    
    xmat = ModelMatrix( ModelFrame(y_form, df) ).m
    if verbose println("Xmat dim:\nN = ", size(xmat, 1), ", K = ", size(xmat, 2)) end
    
    zmat = ModelMatrix( ModelFrame(d_form, df) ).m
    if verbose println("Zmat dim:\nN = ", size(zmat, 1), ", K = ", size(zmat, 2)) end
    
    ## get dimensions
    n = size(y, 1)
    kx = size(xmat, 2)
    kz = size(zmat, 2)
    ktot = 2*kx + kz
    
    if scale_data
        if verbose println("Scaling data...") end
        y = scale_input(y)
        xmat[:,2:kx] = scale_input(xmat[:,2:kx])
        zmat[:,2:kx] = scale_input(zmat[:,2:kx])
    end
    
    ## construct block-diag H
    Hmat = blkdiag(sparse(zmat), sparse(xmat), sparse(xmat))
    if verbose println("Hmat dim:\nN = ", size(Hmat, 1), ", K = ", ktot) end
    
    dim_tup = DimTuple(n=n, kx=kx, kz=kz, ktot=ktot)
    
    ## truncated normal support for latent data
    lower = ifelse( d .== 1, 0, -Inf )
    upper = ifelse( d .== 1, Inf, 0 )
    
    ## collect data objects
    data_in = GibbsData(y=y, d=d, d_l=lower, d_u=upper, xmat=xmat, zmat=zmat, Hmat=Hmat)
    
    ## 3. initialize sampler
    dim_tup = DimTuple(n=n, kx=kx, kz=kz, ktot=ktot)
    
    constant_in = GibbsConstant(dim_tup, prior, param)        
    
    ## 7. Initialize values
    ##    - J, dstar, label, betas, Sigma, SigmaInv
    J = prior.prior_dp.J
    
    dstar = zeros(n)
    for i in 1:n
        dstar[i] = rand( TruncatedNormal(0, 1, lower[i], upper[i]) )
    end
    ##dstar = truncnorm(zeros(n), ones(n), d)

    yuse = zeros(3, n)
    yuse[1, :] = dstar
    
    ## assign component membership uniformly
    label = sample(1:J, n)    
    
    if prior.prior_dp.alpha_shape != 0
        alpha_a = prior.prior_dp.alpha_shape
        alpha_b = prior.prior_dp.alpha_rate
        ##alpha = rand( Gamma(alpha_a, 1/alpha_b) )
        alpha = alpha_a*alpha_b # set to expected value
        a_star = alpha_a + J - 1 # shape parameter for updating
        eta = (alpha + 1)/(alpha + 1 + n)
    else
        alpha = prior.prior_dp.alpha        
        eta = 0.0
    end
    
    betas = rand( MvNormal(prior.prior_beta.beta_mu, prior.prior_beta.beta_V), J) # 3 x J
    
    Sigma = NobileWishart(prior.prior_sigma.sigma_rho, prior.prior_sigma.sigma_R, n=J) # 3 x 3 x J

    data_state = StateData(dstar=dstar, ymiss=zeros(n), y1=zeros(n), y0=zeros(n), yuse=yuse)

    dp_state = StateDP(J=J, alpha=alpha, label=label, eta=eta)

    theta_state = StateTheta(betas=betas, Sigma=Sigma)

    state_in = GibbsState(state_data=data_state, state_dp=dp_state, state_theta=theta_state)
    
    init = GibbsInit(data_init=data_in,
                     constant_init=constant_in,
                     state_init=state_in)

    return init

end

## continue iterating a MCMC chain
function dpmixture_chain(out::GibbsOut)
    
    println("Continuing from last state...")
    
    init = out.gibbs_init
    chain_out = dpmixture_gibbs(init)
    
    ## append new out to old

    ## total iterations
    new_M = chain_out.out_tuple.out_M
    out.out_tuple.out_M += new_M
    
    ## update data
    new_data = chain_out.out_tuple.out_data
    out.out_tuple.out_data.dstar_out = hcat(out.out_tuple.out_data.dstar_out,
                                            new_data.dstar_out)
    out.out_tuple.out_data.dstar_out = hcat(out.out_tuple.out_data.dstar_out,
                                            new_data.dstar_out)
    out.out_tuple.out_data.ymiss_out = hcat(out.out_tuple.out_data.ymiss_out,
                                            new_data.ymiss_out)
    out.out_tuple.out_data.y1_out = hcat(out.out_tuple.out_data.y1_out,
                                         new_data.y1_out)
    out.out_tuple.out_data.y0_out = hcat(out.out_tuple.out_data.y0_out,
                                         new_data.y0_out) 
    out.out_tuple.out_data.y_out = hcat(out.out_tuple.out_data.y_out,
                                        new_data.y_out)
    
    ## update DP
    new_dp = chain_out.out_tuple.out_dp
    out.out_tuple.out_dp.J_out = vcat(out.out_tuple.out_dp.J_out,
                                      new_dp.J_out)
    out.out_tuple.out_dp.label_out = hcat(out.out_tuple.out_dp.label_out,
                                          new_dp.label_out)    
    out.out_tuple.out_dp.alpha_out = vcat(out.out_tuple.out_dp.alpha_out,
                                          new_dp.alpha_out)
    out.out_tuple.out_dp.eta_out = vcat(out.out_tuple.out_dp.eta_out,
                                        new_dp.eta_out)    
    
    ## update theta
    new_theta = chain_out.out_tuple.out_theta
    out.out_tuple.out_theta.betas_out = vcat(out.out_tuple.out_theta.betas_out,
                                             new_theta.betas_out)
    out.out_tuple.out_theta.Sigma_out = vcat(out.out_tuple.out_theta.Sigma_out,
                                             new_theta.Sigma_out)
    
    return out

end

## call gibbs sampler
function dpmixture{T<:GibbsType}(in::T)

    if typeof(in) == GibbsOut
        ## *CALL Gibbs and append new output to old
        out = dpmixture_chain(in)
    elseif typeof(in) == GibbsInit
        out = dpmixture_gibbs(in)
    end

    return out

end

