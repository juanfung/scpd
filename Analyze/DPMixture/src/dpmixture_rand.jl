## Use posterior to sample new latent data at given x_new, z_new

## random draws from ppd, 1 per MCMC draw
function dpmixture_rand(out::GibbsOut, z_new::Array{Float64,1}, x_id::UnitRange)

    ## INPUTS:
    ## out::GibbsOut : Gibbs sampler output    
    ## z_new::Vector : predictors for new selection equation
    ## x_id::UnitRange : indices for outcome equation predictors in z_new, ie, x_new = z_new[x_id]
    
    ## OUTPUT:
    ## ynew_out::Vector : M total samples from posterior, where M = total MCMC draws
    
    pdfi = Distributions.logpdf
    cdfi = Distributions.logcdf
    
    ## constants
    M = out.out_tuple.out_M
    
    ## prior parameters    
    rho = out.gibbs_init.constant_init.prior.prior_sigma.sigma_rho
    R = out.gibbs_init.constant_init.prior.prior_sigma.sigma_R
    beta_mu = out.gibbs_init.constant_init.prior.prior_beta.beta_mu
    beta_V = out.gibbs_init.constant_init.prior.prior_beta.beta_V
    
    ## predictors
    hnew = blkdiag(sparse(z_new'), sparse(z_new[x_id]'), sparse(z_new[x_id]')) # 3 x ktot
    
    ## pre-allocate storage
    ynew_out = Array(Float64, 0) #zeros(M)    
    ynew = Array(Float64, 0)
    w = Array(Float64, 0)
    
    d = Distributions.Normal()
    dz = Distributions.Normal()
    
    for m in 1:M
        
        alpha = out.out_tuple.out_dp.alpha_out[m]
        J = out.out_tuple.out_dp.J_out[m]
        label = out.out_tuple.out_dp.label_out[:,m]
        njs = StatsBase.counts(label, 1:J)
        betas = out.out_tuple.out_theta.betas_out[m]
        Sigma = out.out_tuple.out_theta.Sigma_out[m]
        
        for j in 1:J
            hb = hnew*betas[:,j]
            append!( ynew, hb + chol(Sigma[:,:,j])'*randn(3) )
            push!( w, njs[j]*exp(logpdf(MvNormal(hb, Sigma[:,:,j]), ynew[(3*(j-1)+1):3*j])) )
        end
        
        ## draw from prior
        Sigma = NobileWishart(rho, rho*R)
        betas = rand( Distributions.MvNormal( beta_mu, beta_V ) )
        hb = hnew*betas
        append!( ynew, hb + chol(Sigma)'*randn(3) )
        push!( w, alpha*exp(logpdf(MvNormal(hb, Sigma), ynew[(3*J+1):3*(J+1)])) )
        
        jm = findmax(w)[2]
        append!( ynew_out, ynew[(3*(jm-1)+1):3*jm] )
        
        ##ynew = reshape(ynew, 3, J+1)
        ##append!( ynew_out, ynew[:, findmax(w)[2] ] )
        ##ynew_out[:,m] = ynew[ findmax(w)[2] ]
        
        ## reset storage
        resize!(ynew, 0)
        resize!(w, 0)
        
    end
    
    return reshape(ynew_out, 3, M)
    
end


## for each MCMC draw, sample outN latent data vectors



## compute ppd for ATE, TT, given ynew_out = dpmixture_rand(out, xnew, znew)
function ppd_pdf(ateNew::Array{Float64})
    ## atePDF = KernelDensity.kde(ateNew)
end


## compute cdf at y* for ATE, TT, given ynew_out = dpmixture_rand(out, xnew, znew)
function ppd_cdf(ateNew::Array{Float64}, ateStar::Float64)
    ## ateCDF = StatsBase.ecdf(ateNew)
end
