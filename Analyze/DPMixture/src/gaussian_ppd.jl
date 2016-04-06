## Posterior predictive densities for Guassian model

function gaussian_lppd(out::GibbsOut, z_new::Array{Float64,1}, x_id::UnitRange{Int64};
                       grid::Range=linspace(-10, 10, 100), sums=true)

    ## INPUTS:
    ## out::GibbsOut : Gibbs sampler output
    ## z_new::Vector : predictors for new selection equation
    ## x_id::UnitRange : indices for outcome equation predictors in z_new, ie, x_new = z_new[x_id]
    ## grid::Range : points at which to evaluate ppd
    ## sums::Bool : if true, return sums over M draws; else return all
    
    ## OUTPUT:
    ## ppd::PPD : grid, ppd's evaluated at grid
    
    pdfi = Distributions.logpdf!
    cdfi = Distributions.logcdf!
       
    ## constants
    M = out.out_tuple.out_M
    kx = out.gibbs_init.constant_init.dim.kx
    kz = out.gibbs_init.constant_init.dim.kz
    ktot = out.gibbs_init.constant_init.dim.ktot
    
    ## number of grid points
    gridN = size(grid, 1)
    ##grid = linspace(-10, 10, gridN)
    x_new = sub(znew, x_id)
    
    ## pre-allocate storage
    ate_out = zeros(gridN, M)
    tt_out = zeros(gridN, M)
    late_out = zeros(gridN, M)
    
    dz = Distributions.Normal()
    w = Array(Float64, gridN)
    h1 = Array(Float64, gridN)
    h2 = Array(Float64, gridN)
    
    for m in 1:M
        
        betas = out.out_tuple.out_theta.betas_out[m]
        Sigma = out.out_tuple.out_theta.Sigma_out[m]
        
        bD = sub(betas, 1:kz, 1)
        b1 = sub(betas, kz+1:kz+kx, 1)
        b0 = sub(betas, kz+kx+1:ktot, 1)
        
        xb = dot(x_new, (b1 - b0))
        zb = dot(z_new, bD)
        
        g2 = (Sigma[2,2] + Sigma[3,3] - 2*Sigma[2,3])
        g1 = (Sigma[1,2] - Sigma[1,3]) # sig1D - sig0D
        
        ## ATE
        pdfi(w, Distributions.Normal( xb, sqrt(g2) ), grid ) 
        
        ## TT
        hz = ( zb + (g1/g2)*(grid - xb) )/sqrt( 1 - (g1^2)/g2 )
        cdfi(h1, dz, hz )
        cdfi(h2, dz, fill(zb, gridN) )
        ##h1[:,j] += w[:,j] - h2[:,j]            
        
        h1 += w - h2
        
        for i in 1:gridN
            ## compute ppd ATE at grid point i, for draw m
            ate_out[i,m] = exp(w[i])
            
            ## compute ppd TT at grid point i, for draw m
            tt_out[i,m] = exp(h1[i])
        end
                
    end

    if sums
        ## 3. ppd(ate_i) = sum_m p(ate_i |theta_i) / M
        ate_out = sum(ate_out, 2)/M
        tt_out = sum(tt_out, 2)/M
        late_out = sum(late_out, 2)/M
    end
    
    return PPD(grid = grid, ate = ate_out, tt = tt_out, late = late_out)
    
end
