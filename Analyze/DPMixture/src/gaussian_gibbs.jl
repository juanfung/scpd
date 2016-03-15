## Gibbs sampler for benchmark Gaussian model

@debug function gaussian_gibbs(init::GibbsInit)
    
    ## get data
    const data = init.data_init
    const y = data.y.a
    const d = data.d
    const lower = data.d_l
    const upper = data.d_u
    const xmat = data.xmat.a
    const zmat = data.zmat.a
    const Hmat = data.Hmat
    
    ## get constants
    const dim = init.constant_init.dim
    const n = dim.n
    const kx = dim.kx
    const kz = dim.kz
    const ktot = dim.ktot
    const epsilon = eps(Float64)
    
    const param = init.constant_init.param
    const M = param.M
    const scale_data = param.scale_data
    const verbose = param.verbose
    
    ## get prior settings
    const beta_prior = init.constant_init.prior.prior_beta
    const beta_mu = beta_prior.beta_mu
    const beta_nu = beta_prior.beta_nu
    const beta_V = beta_prior.beta_V
    I_K = eye(ktot)
    spI_K = speye(ktot)
    
    ##beta_VInv = beta_V\I_K
    beta_VInv = \(sparse(beta_V), spI_K)
    
    priorpart = *(beta_VInv, beta_mu)
    
    const sigma_prior = init.constant_init.prior.prior_sigma
    const rho = sigma_prior.sigma_rho
    const R = sigma_prior.sigma_R
    
    ## pre-allocate storage
    data_out = OutData( dstar_out=zeros(n, M),
                       ymiss_out=zeros(n, M),
                       y1_out=zeros(n, M),
                       y0_out=zeros(n, M),
                       y_out=zeros(n, M) )

    theta_out = OutTheta( betas_out=Array(Array{Float64,2}, M),
                          Sigma_out=Array(Array{Float64,2}, M) )
        
    ## get state
    state = init.state_init
    
    chain_state = state.chain
    
    batch_n = state.batch_n
    
    batch_m = state.batch_m
    
    ## init latent data
    data_state = state.state_data
    
    dstar = data_state.dstar
    ymiss = data_state.ymiss
    y1 = data_state.y1
    y0 = data_state.y0
    yuse = data_state.yuse    
    
    ## init component parameters
    theta_state = state.state_theta
    
    betas = theta_state.betas
    Sigma = theta_state.Sigma
    I_3 = eye(3)
    SigmaInv = Sigma\I_3

    zth = zmat*betas[1:kz,:] # n x 1
    xb1 = xmat*betas[(kz+1):(kz+kx),:] # n x 1
    xb0 = xmat*betas[(kz+kx+1):ktot,:] # n x 1    
    
    ## begin the sampler
    if verbose @printf("Batch %d\nBegin sampler...", batch_n) end
    
    for m in 1:M
        
        ## 1. update parameters
        if verbose && floor(M/m) == M/m
            @printf("\nIteration: %d\nUpdating parameters...", m+batch_m)
        end
        
        ## 1a. draw covariance matrix
        eD = dstar - zth # n x 1
        e1 = y1 - xb1 # n x 1
        e0 = y0 - xb0 # n x 1
        
        R_hat = hcat(eD, e1, e0) # n x 3
        R_hat = R_hat'*R_hat # 3 x 3
        R_hat = R_hat + rho*R
        
        Sigma = NobileWishart((n + rho), R_hat)
        SigmaInv = Sigma\I_3
        
        ## Debugging with true Sigma
        ##Sigma = SigmaTrue
        
        ## 1b. draw coefficient vectors
        
        yusei = vcat(dstar, y1, y0) # 3n x 1
        
        sigXi = kron( SigmaInv, speye(n) ) # 3n x 3n
        
        xmatpart = Hmat'*sigXi # Ktot x 3n
        
        covmatpart = xmatpart*Hmat + beta_VInv # Ktot x Ktot
        
        ##covmatpart = covmatpart\I_K # Ktot x Ktot
        ## better approach?
        covmatpart = cholfact(Symmetric(covmatpart, :U))
        
        ymatpart = xmatpart*yusei # Ktot x 1
        
        ##meanpart = covmatpart*(ymatpart + priorpart) # Ktot x 1
        meanpart = covmatpart\(ymatpart + priorpart)
        
        ##betas = meanpart + chol(covmatpart)'*randn(ktot)
        betas = meanpart + (covmatpart[:U]\spI_K)*randn(ktot)
        ##betas = rand(MvNormal(meanpart, covmatpart))
        
        ## 2. draw latent data through data augmentation        
        if verbose && floor(M/m) == M/m @printf("\nUpdating latent data...") end
        
        ## 2a. extract coefficients
        zth = zmat*betas[1:kz,:] # n x 1
        xb1 = xmat*betas[(kz+1):(kz+kx),:] # n x 1
        xb0 = xmat*betas[(kz+kx+1):ktot,:] # n x 1        
        
        sig1 = Sigma[2,2]
        sig0 = Sigma[3,3]
        sig1D = Sigma[1,2]
        sig0D = Sigma[1,3]
        sig10 = Sigma[2,3]
        
        denom1 = (sig1) - (sig1D^2)
        denom0 = (sig0) - (sig0D^2)
        mid = (sig10)*(sig0D)*(sig1D)
        
        ## 2b. draw missing outcomes
        
        mu1 = xb1 +
        (dstar - zth)*( ( (sig0)*(sig1D) - (sig10)*(sig0D) )/denom0 ) +
        (y - xb0)*( ( (sig10) - (sig0D)*(sig1D) )/denom0 )
        
        mu0 = xb0 +
        (dstar - zth)*( ( (sig1)*(sig0D) - (sig10)*(sig1D) )/denom1 ) +
        (y - xb1)*( ( (sig10) - (sig0D)*(sig1D) )/denom1 )
        
        omega1 = (sig1) - ( ( (sig1D^2)*(sig0) - 2*mid + (sig10^2) )/denom0 )
        
        omega0 = (sig0) - ( ( (sig0D^2)*(sig1) - 2*mid + (sig10^2) )/denom1 )
        
        mu_miss = (1 .- d).*mu1 + d.*mu0
        
        var_miss = (1 .- d)*omega1 + d*omega0
        
        ymiss = mu_miss + sqrt(var_miss).*randn(n)
        ##ymiss = rand(Normal(mu_miss, sqrt(var_miss)), n)
        
        ## 2c. draw selection outcome
        
        denomd = (sig1)*(sig0) - (sig10^2)
        
        y1 = d.*y + (1 .- d).*ymiss
        y0 = d.*ymiss + (1 .- d).*y
        
        sd1 = ( ( (sig0)*(sig1D) - (sig10)*(sig0D) )/denomd )
        sd0 = ( ( (sig1)*(sig0D) - (sig10)*(sig1D) )/denomd )
        
        mu_d = zth + (y1 - xb1)*sd1 + (y0 - xb0)*sd0
        
        omega_d = 1 - ( ( (sig1D^2)*(sig0) - 2*mid + (sig0D^2)*(sig1) )/denomd )
        
        @bp omega_d < 0
        
        for i in 1:n
            dstar[i] = rand(TruncatedNormal(mu_d[i], sqrt(omega_d), lower[i], upper[i]) )
        end
        ##dstar = truncnorm(mu_d, sqrt(omega_d)*ones(n), d) # or repmat([sqrt(omega_d)], n)
        
        @bp count(ds -> ds==Inf, dstar) > 0
        
        ## 2d. store agumented data
        yuse[1,:] = dstar
        yuse[2,:] = y1
        yuse[3,:] = y0        
        
        ## 3. save iteration m draws
        if verbose && floor(M/m) == M/m @printf("\nDone!") end
        
        ## save augmented data
        data_out.dstar_out[:,m] = dstar
        data_out.ymiss_out[:,m] = ymiss
        data_out.y1_out[:,m] = y1
        data_out.y0_out[:,m] = y0
        data_out.y_out[:,m] = d.*y1 + (1 .- d).*y0
        ##@bp data_out.y_out[:,m] != mean(y)
        
        ## save component parameters
        theta_out.betas_out[m] = reshape(betas, ktot, 1)
        theta_out.Sigma_out[m] = Sigma
        
    end

    if verbose @printf("\nSampler run complete.\n") end
    
    ## collect output
    M_out = M + batch_m
    out = OutTuple(out_M=M, out_data=data_out, out_dp=OutDP(), out_theta=theta_out)
    
    data_state = StateData(dstar=vec(dstar), ymiss=vec(ymiss), y1=vec(y1), y0=vec(y0), yuse=yuse)
    theta_state = StateTheta(betas=betas, Sigma=Sigma)
    chain_state = true
    
    init.state_init = GibbsState(state_data=data_state,
                                 state_dp=StateDP(),
                                 state_theta=theta_state,
                                 chain=chain_state,
                                 batch_n=batch_n,
                                 batch_m=M_out)
    
    gibbs_out = GibbsOut(out, init)
    
    return gibbs_out

end
