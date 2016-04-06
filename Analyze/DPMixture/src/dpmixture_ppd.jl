## Posterior predictive densities for DP Mixture

## 1. ATE|x,param,Data ~ N( x^{T}(b_{1}-b_{0}), g_{2} ), g_{2}=\sigma_{1}^{2}+\sigma_{2}^{2}-\sigma_{1}\sigma_{2}
## 2. TT|x,z,param,Data ~ [ p(ATE|x,param,Data)/Pr( N(0,1) < z^{T}bD) ]*[ Pr( N(0,1) < h(z) ) ]
##    where h(z) = [ z^{T}bD + (g1/g2)( y1-y0 - x^{T}(b1-b0) ) ]/[ sqrt( 1 - g1^2/g2) ]
##     and  g1 = \sigma_{1D} - \sigma_{0D}
## 3. LATE|x,z,z',Data ~ [ p(ATE|x,param,Data)/[ Pr( N(0,1) < z'^{T}bD ) - Pr( N(0,1) < z^{T}bD ) ]*A
##    where A = h(z') - h(z)

## TODO:
## let x_new = index/range of x in z_new: x_new::Union{UnitRange{Int64},Array{Float64}}
## then, x_new = sub(z_new, :, idx)

## TODO:
## write to file, or use mmap_array

## vectorized calls to logpdf/logcdf
function dpmixture_lppd(out::GibbsOut, z_new::Array{Float64,1}, x_id::UnitRange;
                        grid::Range=linspace(-10, 10, 100), sums::Bool=true)

    ## INPUTS:
    ## out::GibbsOut : Gibbs sampler output
    ## z_new::Vector : predictors for new selection equation
    ## x_id::UnitRange : indices for outcome equation predictors in z_new, ie, x_new = z_new[x_id]
    ## grid::Range : points at which to evaluate ppd
    ## sums::Bool : if true, return sums over M draws; else return all

    ## OUTPUT:
    ## ppd::PPD : grid, ppd's evaluated at grid
    
    pdfi = Distributions.logpdf
    cdfi = Distributions.logcdf
       
    ## constants
    M = out.out_tuple.out_M
    N = out.gibbs_init.constant_init.dim.n
    kx = out.gibbs_init.constant_init.dim.kx
    kz = out.gibbs_init.constant_init.dim.kz
    ktot = out.gibbs_init.constant_init.dim.ktot
    
    ## prior parameters    
    rho = out.gibbs_init.constant_init.prior.prior_sigma.sigma_rho
    R = out.gibbs_init.constant_init.prior.prior_sigma.sigma_R
    beta_mu = out.gibbs_init.constant_init.prior.prior_beta.beta_mu
    beta_V = out.gibbs_init.constant_init.prior.prior_beta.beta_V
    
    ## number of grid points
    gridN = size(grid, 1)
    ##grid = linspace(-10, 10, gridN)
    x_new = sub(znew, x_id)
    
    ## pre-allocate storage
    ate_out = zeros(gridN, M)
    tt_out = zeros(gridN, M)
    late_out = zeros(gridN, M)
    
    dz = Distributions.Normal()
    
    for m in 1:M
        
        alpha = out.out_tuple.out_dp.alpha_out[m]
        J = out.out_tuple.out_dp.J_out[m]
        label = out.out_tuple.out_dp.label_out[:,m]
        njs = StatsBase.counts(label, 1:J)
        betas = out.out_tuple.out_theta.betas_out[m]
        Sigma = out.out_tuple.out_theta.Sigma_out[m]

        w = Array(Float64, 0)
        h1 = Array(Float64, 0)
        h2 = Array(Float64, 0)        
        
        for j in 1:J
            bD = betas[1:kz,j]
            b1 = betas[kz+1:kz+kx,j]
            b0 = betas[kz+kx+1:ktot,j]
            
            xb = dot(x_new, (b1 - b0))
            zb = dot(z_new, bD)
            
            g2 = (Sigma[2,2,j] + Sigma[3,3,j] - 2*Sigma[2,3,j])
            g1 = (Sigma[1,2,j] - Sigma[1,3,j]) # sig1D - sig0D
            
            ## ATE
            append!( w, pdfi(Distributions.Normal( xb, sqrt(g2) ), grid )) 
            
            ## TT
            hz = ( zb + (g1/g2)*(grid - xb) )/sqrt( 1 - (g1^2)/g2 )
            append!( h1, cdfi( dz, hz ) )
            append!( h2, cdfi( dz, fill(zb, gridN) ) )
            ##h1[:,j] += w[:,j] - h2[:,j]
            
        end
        
        ## draw from prior
        Sigma = NobileWishart(rho, rho*R)
        betas = rand( Distributions.MvNormal( beta_mu, beta_V ) )
        bD = betas[1:kz]
        b1 = betas[kz+1:kz+kx]
        b0 = betas[kz+kx+1:ktot]
        xb = dot(x_new, b1-b0)
        zb = dot(z_new, bD)
        g2 = (Sigma[2,2] + Sigma[3,3] - 2*Sigma[2,3])
        g1 = (Sigma[1,2] - Sigma[1,3])
        
        ## ATE
        append!(w, pdfi(Distributions.Normal( xb, sqrt(g2) ) , grid ) )
        
        ## TT
        hz = ( zb + (g1/g2)*(grid - xb) )/sqrt( 1 - (g1^2)/g2 )
        append!( h1, cdfi( dz, hz ) )
        append!( h2, cdfi( dz, fill(zb, gridN) ) )
        ##h1[:,J+1] += w[:,J+1] - h2[:,J+1]
        
        h1 += w - h2

        w = reshape(w, gridN, J+1)
        h1 = reshape(h1, gridN, J+1)
        
        for i in 1:gridN
            ## compute ppd ATE at grid point i, for draw m
            ate_out[i,m] = ( sum(njs.*exp(w[i,1:J]')) + alpha*exp(w[i,J+1]) )/(N + alpha)
            
            ## compute ppd TT at grid point i, for draw m
            tt_out[i,m] = ( sum(njs.*exp(h1[i,1:J]')) + alpha*exp(h1[i,J+1]) )/(N + alpha)
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


## function to plot ate ppd using Gadfly
function plot_ppd(ppd::PPD, sums=true)
    if sums
        Gadfly.plot(x=collect(ppd.grid), y=ppd.ate, Geom.line)
    else
        M = size(ppd.ate, 2)
        Gadfly.plot(x=collect(ppd.grid), y=sum(ppd.ate, 2)/M, Geom.line)
    end
end


