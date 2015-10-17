## (Marginalized) Polya Urn Gibbs sampler for DPM

##@debug
function dpmixture_gibbs(init::GibbsInit)
    
    ## get data
    data = init.data_init
    const y = data.y
    const d = data.d
    const lower = data.d_l
    const upper = data.d_u
    const xmat = data.xmat
    const zmat = data.zmat
    const Hmat = data.Hmat
    
    ## get constants
    dim = init.constant_init.dim
    const n = dim.n
    const kx = dim.kx
    const kz = dim.kz
    const ktot = dim.ktot
    const epsilon = eps(Float64)
    
    param = init.constant_init.param
    const M = param.M
    const scale_data = param.scale_data
    const verbose = param.verbose
    
    ## get prior settings
    dp_prior = init.constant_init.prior.prior_dp
    const alpha_a = dp_prior.alpha_shape
    const alpha_b = dp_prior.alpha_rate
    
    beta_prior = init.constant_init.prior.prior_beta
    const beta_mu = beta_prior.beta_mu
    const beta_nu = beta_prior.beta_nu
    const beta_V = beta_prior.beta_V
    I_K = eye(ktot)
    beta_VInv = beta_V\I_K
    priorpart = *(beta_VInv, beta_mu)
    
    sigma_prior = init.constant_init.prior.prior_sigma
    const rho = sigma_prior.sigma_rho
    const R = sigma_prior.sigma_R
    
    ## pre-allocate storage
    data_out = OutData( dstar_out=zeros(n, M),
                       ymiss_out=zeros(n, M),
                       y1_out=zeros(n, M),
                       y0_out=zeros(n, M),
                       y_out=zeros(n, M) )
    
    dp_out = OutDP( J_out=zeros(Int64, M), label_out=zeros(Int64, n, M),
                   alpha_out=zeros(M), eta_out=zeros(M) )
    
    theta_out = OutTheta( betas_out=Array(Array{Float64,2}, M),
                         Sigma_out=Array(Array{Float64,3}, M) )
    
    ## get state
    state = init.state_init
    
    chain_state = state.chain
    
    ## init latent data
    data_state = state.state_data
    
    dstar = data_state.dstar
    ymiss = data_state.ymiss
    y1 = data_state.y1
    y0 = data_state.y0
    yuse = data_state.yuse
    
    ## init DP parameters
    dp_state = state.state_dp
    
    J = dp_state.J
    alpha = dp_state.alpha
    label = dp_state.label
    eta = dp_state.eta
    
    zdenom = alpha + n - 1
    w_hat = zeros(J)
    w_new = 0.0
    
    ## count component membership
    ##counts = [count(z->z==j, label) for j in 1:J]
    njs = StatsBase.counts(label, 1:J)
    
    ## init component parameters
    theta_state = state.state_theta
    
    betas = theta_state.betas
    Sigma = theta_state.Sigma
    I_3 = eye(3)
    SigmaInv = mapslices(s->s\I_3, Sigma, [1, 2])
    
    ## begin the sampler
    if verbose @printf("Initial J = %d\nInitial alpha = %f\nBegin sampler...", J, alpha) end
    
    for m in 1:M

        ## 1. update labels
        if verbose && floor(M/m) == M/m @printf("\nIteration: %d\nUpdating labels...", m) end

        ## sample component label for each i
        for i in 1:n
            ji = label[i] # current cluster
            label[i] = 0 # remove cluster
            njs[ji] = njs[ji] - 1 # decrease count in cluster ji
            ## REMOVE EMPTY COMPONENT!
            if njs[ji] == 0
                ## remove component parameters
                ##Sigma = Sigma[:,:,1:J.!=ji]
                ##SigmaInv = SigmaInv[:,:,1:J.!=ji]
                Sigma = Sigma[:,:,vcat(1:ji-1, ji+1:J)]
                SigmaInv = SigmaInv[:,:,vcat(1:ji-1, ji+1:J)]
                betas = betas[:,1:J.!=ji]
                njs = deleteat!(njs, ji)
                ## shift labels
                idx = find(z -> z>ji, label)
                label[idx] = label[idx] - 1
                J = J - 1
            end
            
            Hi = Hmat[vcat(i, i+n, i+2*n), :] # 3 x Ktot
            
            ## prob of sampling existing component
            w_hat = resize!(w_hat, J)
            ##w_hat = map(j-> logpdf(MvNormal(Hi*betas[:,j], Sigma[:,:,j]), yuse[:,i]), 1:J)
            for j in 1:J
                w_hat[j] = logpdf(MvNormal(Hi*betas[:,j], Sigma[:,:,j]), yuse[:,i])
            end
            w_hat = (njs/zdenom).*exp(w_hat)
            
            ## probability of sampling new component
            ## sample sigma
            R_hat = yuse[:,i] - Hi*beta_mu # 3 x nj
            R_hat = R_hat*R_hat' # 3 x 3
            R_hat = R_hat + rho*R
            Sigma_j = NobileWishart(rho+1, R_hat)
            SigmaInv_j = Sigma_j\I_3
            ## sample betas
            xmatpart = Hi'*SigmaInv_j # Ktot x 3            
            covmatpart = xmatpart*Hi + beta_VInv # Ktot x Ktot
            covmatpart = covmatpart\I_K # Ktot x Ktot
            ymatpart = xmatpart*yuse[:,i] # Ktot x 1
            meanpart = covmatpart*( ymatpart + priorpart ) # Ktot x 1
            betas_j = meanpart + chol(covmatpart)'*randn(ktot)
            
            w_new = logpdf(MvNormal(Hi*betas_j, Sigma_j), yuse[:,i])
            w_new = (alpha/zdenom)*exp(w_new)
            
            w = vcat(w_hat, w_new)
            w = max(w, epsilon)
            w = w/sum(w)
            
            ji = rand( Categorical(w) )
            label[i] = ji
            
            if ji == J + 1
                ## update J
                J = J + 1
                ji = J
                njs = push!(njs, 0)
                ## update components
                Sigma = cat(3, Sigma, Sigma_j)
                SigmaInv = cat(3, SigmaInv, SigmaInv_j)
                betas = hcat(betas, betas_j) # [betas betas_j]
            end
            
            ## update counts
            njs[ji] = njs[ji] + 1
            
        end
        ##@bp size(Sigma, 3) > J
        ##@bp size(Sigma, 3) > J
        
        if verbose && floor(M/m) == M/m @printf("\nCurrent J = %d", J) end
        
        ## update counts
        ##njs = StatsBase.counts(label, 1:J)
        
        ## extract coefficients
        betaD = betas[1:kz,:]
        beta1 = betas[(kz+1):(kz+kx),:]
        beta0 = betas[(kz+kx+1):ktot,:]
        
        sig1 = map(j -> sqrt(Sigma[2,2,j]), 1:J)
        sig0 = map(j -> sqrt(Sigma[3,3,j]), 1:J)
        sig1D = map(j -> Sigma[1,2,j], 1:J)
        sig0D = map(j -> Sigma[1,3,j], 1:J)
        sig10 = map(j -> Sigma[2,3,j], 1:J)
        
        ## 2. update latent data and component parameters
        if verbose && floor(M/m) == M/m @printf("\nUpdating data and parameters...") end
        
        for j in 1:J
            
            nj = njs[j]
            
            if nj == 0 continue end
            
            idx = find(z -> z==j, label)
            
            dstarj = dstar[idx]
            yj = y[idx]
            dj = d[idx]
            
            Hj = Hmat[vcat(idx, idx+n, idx+2*n),:] # 3nj x ktot
            
            zth = zmat[idx,:]*betaD[:,j] # nj x 1
            xb1 = xmat[idx,:]*beta1[:,j] # nj x 1
            xb0 = xmat[idx,:]*beta0[:,j] # nj x 1
            
            denom1 = (sig1[j])^2 - (sig1D[j])^2
            denom0 = (sig0[j])^2 - (sig0D[j])^2
            middle = (sig10[j])*(sig0D[j])*(sig1D[j])
            
            ## 2a. draw missing outcomes
            
            mu1 = xb1 +
            (dstarj - zth)*( ( (sig0[j])^2*(sig1D[j]) - (sig10[j])*(sig0D[j]) )/denom0 ) +
            (yj - xb0)*( ( (sig10[j]) - (sig0D[j])*(sig1D[j]) )/denom0 )
            
            mu0 = xb0 +
            (dstarj - zth)*( ( (sig1[j])^2*(sig0D[j]) - (sig10[j])*(sig1D[j]) )/denom1 ) +
            (yj - xb1)*( ( (sig10[j]) - (sig0D[j])*(sig1D[j]) )/denom1 )
            
            omega1 = (sig1[j])^2 -
            ( ( (sig1D[j])^2*(sig0[j])^2 - 2*middle + (sig10[j])^2 )/denom0 )
            
            omega0 = (sig0[j])^2 -
            ( ( (sig0D[j])^2*(sig1[j])^2 - 2*middle + (sig10[j])^2 )/denom1 )
            
            mu_miss = (1 .- dj).*mu1 + dj.*mu0
            
            var_miss = (1 .- dj)*omega1 + dj*omega0
            
            ymiss_j = mu_miss + sqrt(var_miss).*randn(nj)
            ##ymiss_j = rand(Normal(mu_miss, sqrt(var_miss)), nj)
            
            ymiss[idx] = ymiss_j
                        
            ## 2b. draw latent data
            
            denomd = (sig1[j]^2)*(sig0[j]^2) - (sig10[j]^2)
            
            y1j = dj.*yj + (1 .- dj).*ymiss_j
            y0j = dj.*ymiss_j + (1 .- dj).*yj
            
            sd1 = ( ( (sig0[j])^2*(sig1D[j]) - (sig10[j])*(sig0D[j]) )/denomd )
            sd0 = ( ( (sig1[j])^2*(sig0D[j]) - (sig10[j])*(sig1D[j]) )/denomd )
            
            mu_d = zth + (y1j - xb1)*sd1 + (y0j - xb0)*sd0            
            
            omega_d = 1 -
            ( ( (sig1D[j])^2*(sig0[j])^2 - 2*middle + (sig0D[j])^2*(sig1[j])^2 )/denomd )
            
            ##@bp omega_d < 0
            
            for i in 1:nj
                dstarj[i] = rand(TruncatedNormal(mu_d[i],
                                                 sqrt(omega_d),
                                                 lower[idx[i]],
                                                 upper[idx[i]]) )
            end
            ##dstarj = truncnorm(mu_d, repmat([sqrt(omega_d)], nj), dj) # or sqrt(omega_d)*ones(n)
            
            ##@bp count(d->d==Inf, dstarj) > 0
            
            ## save latent data
            dstar[idx] = dstarj
            
            ## save missing data
            y1[idx] = y1j
            y0[idx] = y0j
            
            ## 2c. draw error covariance matrix
            
            eD = dstarj - zth # nj x 1
            e1 = y1j - xb1 # nj x 1
            e0 = y0j - xb0 # nj x 1
            
            R_hat = hcat(eD, e1, e0) # nj x 3
            R_hat = R_hat'*R_hat # 3 x 3
            R_hat = R_hat + rho*R
            
            Sigma_j = NobileWishart((nj + rho), R_hat)
            SigmaInv_j = Sigma_j\I_3
            
            ## Debugging with true Sigma
            ##Sigma_j = SigmaTrue
            
            Sigma[:,:,j] = Sigma_j
            SigmaInv[:,:,j] = SigmaInv_j
            
            ## 2d. draw coefficient vectors
            
            yusej = vcat(dstarj, y1j, y0j) # 3nj x 1
            
            sigXi = kron( SigmaInv_j, eye(nj) ) # 3nj x 3nj
            
            xmatpart = Hj'*sigXi # Ktot x 3nj
            
            covmatpart = xmatpart*Hj + beta_VInv # Ktot x Ktot
            
            covmatpart = covmatpart\I_K # Ktot x Ktot
            ## better approach?
            
            ymatpart = xmatpart*yusej # Ktot x 1
            
            meanpart = covmatpart*(ymatpart + priorpart) # Ktot x 1
            
            betas_j = meanpart + chol(covmatpart)'*randn(ktot)
            ##betas_j = rand(MvNormal(meanpart, covmatpart))
            betas[:,j] = betas_j
            
        end

        ## 2e. store agumented data
        yuse[1,:] = dstar
        yuse[2,:] = y1
        yuse[3,:] = y0
        
        ## 3. update alpha via Esboar and West (1994) method
        ## Assumes conjugate gamma prior, G(a,b)
        
        if alpha_a !=0
            
            if verbose && floor(M/m) == M/m @printf("\nUpdating alpha...") end

            ## sample auxiliary variable
            eta = rand( Beta(alpha+1, n) )
            b_star = alpha_b - log(eta)
            A = (alpha_a + J - 1)/(n*b_star)
            pi_eta = A/(1 + A)
            a_star = ifelse(pi_eta > 0.5, alpha_a + J, alpha_a + J - 1)
            
            ## sample alpha
            alpha = rand(Gamma(a_star, 1/b_star))
            zdenom = alpha + n - 1
            
        end        
        
        if verbose && floor(M/m) == M/m @printf("\nCurrent alpha = %f", alpha) end
        
        ## 4. save iteration m draws
        if verbose && floor(M/m) == M/m @printf("\nDone!") end

        ## save augmented data
        data_out.dstar_out[:,m] = dstar
        data_out.ymiss_out[:,m] = ymiss
        data_out.y1_out[:,m] = y1
        data_out.y0_out[:,m] = y0
        data_out.y_out[:,m] = d.*y1 + (1 .- d).*y0
        ####@bp data_out.y_out[:,m] != mean(y)
        
        ## save DP parameters
        dp_out.J_out[m] = J
        dp_out.label_out[:,m] = label
        dp_out.alpha_out[m] = alpha
        dp_out.eta_out[m] = eta
        
        ## save component parameters
        theta_out.betas_out[m] = betas
        theta_out.Sigma_out[m] = Sigma

    end

    if verbose @printf("\nSampler run complete.\n") end
    
    ## collect output
    out = OutTuple(out_M=M, out_data=data_out, out_dp=dp_out, out_theta=theta_out)
    
    data_state = StateData(dstar=dstar, ymiss=ymiss, y1=y1, y0=y0, yuse=yuse)
    dp_state = StateDP(J=J, alpha=alpha, label=label, eta=eta)
    theta_state = StateTheta(betas=betas, Sigma=Sigma)
    chain_state = true
    
    init.state_init = GibbsState(state_data=data_state,
                                 state_dp=dp_state,
                                 state_theta=theta_state,
                                 chain=chain_state)
    
    gibbs_out = GibbsOut(out, init)
    
    return gibbs_out
    
end
