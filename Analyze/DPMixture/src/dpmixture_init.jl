## Set up to run/continue Gibbs sampler for DPMs, etc.

## function to select sampler
function set_sampler(model::ASCIIString)

    if model == "dpm"
        f = dpmixture_gibbs
    elseif model == "blocked"
        f = dpmixture_blocked
    elseif model == "fmn"
        f =  fmn_gibbs
    elseif model == "gaussian"
        f = gaussian_gibbs
    end

    return f

end

## initialize sampler
function dpmixture_init(data::DataTuple, prior::PriorTuple, param::ParamTuple)
    
    ## Inputs:
    ## data (data type object): DataFrame and Formula objects
    ## prior (prior type object): prior hyperparameters
    ## param (param type object): sampler parameters
    
    ## Output:
    ## GibbsInit (gibbs init type object): input to sampler
    
    ## 1. get MCMC parameters
    
    const M = param.M    
    const scale_data = param.scale_data
    const verbose = param.verbose
    if verbose
        println("M = ", M)
        println("scale data = ", scale_data)
        println("verbose = ", verbose)
    end

    const beta_mu = prior.prior_beta.beta_mu
    const beta_V = prior.prior_beta.beta_V
    const rho = prior.prior_sigma.sigma_rho*prior.prior_sigma.sigma_rho
    const R = prior.prior_sigma.sigma_rho*prior.prior_sigma.sigma_R
    
    ## 2. get data -> model matrix
    ##    - optional: scale data
    
    const y_form = data.y_form
    const d_form = data.d_form
    const df = data.df
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
    
    ## scale data?
    if true in scale_data
        if scale_data[1]
            if verbose println("Scaling response...") end
            ## scale response?
            ys = standardize(y)
        else
            ys = ScaleData(a=y)
        end
        if scale_data[2]
            if verbose println("Scaling inputs...") end
            ## scale inputs?
            xmats = standardize(xmat)
            zmats = standardize(zmat)
        else
            xmats = ScaleData(a=xmat, m=zeros(kx), s=ones(kx))
            zmats = ScaleData(a=zmat, m=zeros(kz), s=ones(kz))
        end
    end
    
    ## construct block-diag H
    Hmat = blkdiag(sparse(zmats.a), sparse(xmats.a), sparse(xmats.a))
    if verbose println("Hmat dim:\nN = ", size(Hmat, 1), ", K = ", ktot) end
    
    dim_tup = DimTuple(n=n, kx=kx, kz=kz, ktot=ktot)
    
    ## truncated normal support for latent data
    lower = ifelse( d .== 1, 0, -Inf )
    upper = ifelse( d .== 1, Inf, 0 )
    
    ## collect data objects
    data_in = GibbsData(y=ys, d=d, d_l=lower, d_u=upper, xmat=xmats, zmat=zmats, Hmat=Hmat)
    
    ## 3. initialize sampler
    if verbose println("Initializing sampler...") end
    dim_tup = DimTuple(n=n, kx=kx, kz=kz, ktot=ktot)
    
    constant_in = GibbsConstant(dim_tup, prior, param)        
    
    ## 7. Initialize values
    ##    - J, dstar, label, betas, Sigma, SigmaInv
    const J = prior.prior_dp.J
    
    dstar = zeros(n)
    for i in 1:n
        @inbounds dstar[i] = rand( TruncatedNormal(0, 1, lower[i], upper[i]) )
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
        const alpha = prior.prior_dp.alpha
        const eta = 0.0
    end
    
    ## initialize betas
    betas = rand( MvNormal(beta_mu, beta_V), J) # ktot x J
    
    ## initialize Sigma (3 x 3 x J)
    ## NB: if J=1, Sigma is (3 x 3)
    Sigma = NobileWishart(rho, rho*R, n=J)
    
    ## OR, as Array of Arrays:
    ##betas = Array(Array{Float64,1},0)
    ##Sigma = Array(Array{Float64,2},0)
    ##for j in 1:J
    ##    push!(betas, rand( MvNormal(beta_mu, beta_V) ) )
    ##    push!(Sigma, NobileWishart(rho, rho*R))
    ##end
    
    ## collect data
    data_state = StateData(dstar=dstar, ymiss=zeros(n), y1=zeros(n), y0=zeros(n), yuse=yuse)

    ## collect DP hyperparameters
    if J == 1
        dp_state = StateDP()
    else       
        dp_state = StateDP(J=J, alpha=alpha, label=label, eta=eta)
    end
    
    ## collect component hyperparameters
    theta_state = StateTheta(betas=betas, Sigma=Sigma)

    ## collect initial state
    state_in = GibbsState(state_data=data_state, state_dp=dp_state, state_theta=theta_state)

    ## collect init object
    init = GibbsInit(data_init=data_in,
                     constant_init=constant_in,
                     state_init=state_in)

    if verbose println("Ready!") end
    
    return init
    
end

## continue sampler, using last state as init
function dpmixture_chain(out::GibbsOut; model="dpm")

    dpmixture_sampler = set_sampler(model)    
       
    init = out.gibbs_init
    out = out.out_tuple

    println("Continuing from last state...")
    chain_out = dpmixture_sampler(init)
    init = chain_out.gibbs_init
    chain_out = chain_out.out_tuple
    
    ## append new out to old
    
    ## update current number of iterations (old out_M + new out_M)
    new_M = init.state_init.batch_m 
    out.out_M += chain_out.out_M
    ## update batch
    ##new_M = chain_out.gibbs_init.state_init.batch_m
    
    ## update data
    out.out_data.dstar_out = hcat(out.out_data.dstar_out,
                                  chain_out.out_data.dstar_out)
    out.out_data.ymiss_out = hcat(out.out_data.ymiss_out,
                                  chain_out.out_data.ymiss_out)
    out.out_data.y1_out = hcat(out.out_data.y1_out,
                               chain_out.out_data.y1_out)
    out.out_data.y0_out = hcat(out.out_data.y0_out,
                               chain_out.out_data.y0_out) 
    out.out_data.y_out = hcat(out.out_data.y_out,
                              chain_out.out_data.y_out)
    
    chain_out.out_data = OutData() # clear memory
    
    ## update DP
    out.out_dp.J_out = vcat(out.out_dp.J_out,
                            chain_out.out_dp.J_out)
    out.out_dp.label_out = hcat(out.out_dp.label_out,
                                chain_out.out_dp.label_out)    
    out.out_dp.alpha_out = vcat(out.out_dp.alpha_out,
                                chain_out.out_dp.alpha_out)
    out.out_dp.eta_out = vcat(out.out_dp.eta_out,
                              chain_out.out_dp.eta_out)
    
    chain_out.out_dp = OutDP() # clear memory
    
    ## update theta
    out.out_theta.betas_out = vcat(out.out_theta.betas_out,
                                   chain_out.out_theta.betas_out)
    out.out_theta.Sigma_out = vcat(out.out_theta.Sigma_out,
                                   chain_out.out_theta.Sigma_out)
    
    ##chain_out.out_theta = OutTheta() # clear memory
    chain_out = 0
    
    out = GibbsOut( out_tuple = out, gibbs_init=init )
    
    return out

end

## wrapper to call gibbs sampler
## Better(?):
## function dpmixture(in::GibbsInit; model="dpm") dpmixture_sampler(in, model=model)
## function dpmixture(in::GibbsOut; model="dpm") dpmixture_chain(in, model=model)
function dpmixture{T<:GibbsType}(in::T; model="dpm")
    
    if typeof(in) == DPMixture.GibbsOut
        ## *CALL Gibbs and append new output to old
        out = dpmixture_chain(in, model=model)
        ##out = dpmixture_dump(in) ## NB: separates output
    elseif typeof(in) == DPMixture.GibbsInit
        dpmixture_sampler = set_sampler(model)
        out = dpmixture_sampler(in)
    end
    
    return out
    
end

## save output to disk and collect garbage
## return last state to continue sampler
function dpmixture_dump(out::GibbsOut; fname="out", model="dpm")
    
    ## save current state
    init = out.gibbs_init
    
    ## current batch total iterations
    ## total iterations
    ##old_M = out.out_tuple.out_M
    ##out.gibbs_init.state_init.batch_m = old_M
    old_M = init.state_init.batch_m
    
    ## current "batch" number
    batch_n = out.gibbs_init.state_init.batch_n
    
    ## write to disk using JLD:
    ##out_name = "out_$(old_M)"
    out_name = "$(fname)_$(batch_n)"
    
    JLD.jldopen(out_name*".jld", "w") do file
        addrequire(file, DPMixture)
        write(file, out_name, out)
    end
    
    ## clear memory
    out = 0
    gc()
    
    ## update batch number
    init.state_init.batch_n = batch_n + 1
    
    ## continue chain
    ## NB: automatically inherits state chain=true
    out = dpmixture(init, model=model)
    
    ## update runs as (out_m out of out_M)    
    ##out.out_tuple.out_M += old_M
    ##init.state_init.batch_m += old_M
    
    return out

end


## combine output from series of chains
## **use constructors??
## **to read from disk:
## out = load("./out.jld", "out")
function dpmixture_combine(out1::GibbsOut, out2::GibbsOut)
    
    ## input: out1, out2 - GibbsOut objects
    ## output: out - GibbsOut object with out.gibbs_init = out2.gibbs_init

    ## 1. combine out_tuples via vcat/hcat
    ## out = out1.out_tuple
    
    ## 2. clear, gc()
    ## out1 = 0
    ## out2 = 0
    ## gc()

    ## 3. output
    ## out = GibbsOut(out_tuple=out, gibbs_init=out2.gibbs_init)

    ##return out

end
