## Use posterior to sample new latent data at given x_new, z_new

## random draws from ppd, 1 per MCMC draw
function gaussian_rand(out::GibbsOut, z_new::Array{Float64,1}, x_id::UnitRange{Int64})

    ## INPUTS:
    ## out::GibbsOut : Gibbs sampler output
    ## z_new::Vector : predictors for new selection equation
    ## x_id::UnitRange : indices for outcome equation predictors in z_new, ie, x_new = z_new[x_id]
    
    ## OUTPUT:
    ## ynew_out::Vector : M total samples from posterior, where M = total MCMC draws
    ## w::Vector : associated probabilities
    
    ## constants
    M = out.out_tuple.out_M
    
    ## predictors
    hnew = blkdiag(sparse(z_new'), sparse(z_new[x_id]'), sparse(z_new[x_id]')) # 3 x ktot
    
    ## pre-allocate storage
    ynew_out = Array(Float64, 0) #zeros(M)
    w = Array(Float64, 0)
    
    d = Distributions.Normal()
    dz = Distributions.Normal()
    
    for m in 1:M
        
        betas = out.out_tuple.out_theta.betas_out[m]
        Sigma = out.out_tuple.out_theta.Sigma_out[m]
        
        hb = hnew*vec(betas) ## hnew*betas
        append!( ynew_out, hb + chol(Sigma)'*randn(3) )
        push!( w, exp(logpdf(MvNormal(hb, Sigma), ynew_out[(3*(m-1)+1):3*m])) )
        
    end
    
    return (reshape(ynew_out, 3, M), w)
    
end
