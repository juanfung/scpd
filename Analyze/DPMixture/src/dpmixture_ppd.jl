## Posterior predictive densities for DP Mixture

type PosteriorPredictive
    grid::LinSpace{Float64}
    ate::Array{Float64}
    tt::Array{Float64}
    late::Array{Float64}
end

typealias PPD PosteriorPredictive

PPD(;grid=linspace(-2,2,2), ate=zeros(2), tt=zeros(2), late=zeros(2)) = PPD(grid, ate, tt, late)

function dpmixture_lppd(out::GibbsOut, x_new::Array{Float64,1}, z_new::Array{Float64,1}; gridN=100, sums=true)
    
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
    
    const pdfi = Distributions.logpdf
    const cdfi = Distributions.logcdf
       
    ## constants
    const M = out.out_tuple.out_M
    const N = out.gibbs_init.constant_init.dim.n
    const kx = out.gibbs_init.constant_init.dim.kx
    const kz = out.gibbs_init.constant_init.dim.kz
    const ktot = out.gibbs_init.constant_init.dim.ktot
    
    ## prior parameters    
    const rho = out.gibbs_init.constant_init.prior.prior_sigma.sigma_rho
    const R = out.gibbs_init.constant_init.prior.prior_sigma.sigma_R
    const beta_mu = out.gibbs_init.constant_init.prior.prior_beta.beta_mu
    const beta_V = out.gibbs_init.constant_init.prior.prior_beta.beta_V
    
    ## if scaled response, sy = std(y), else sy = 1
    const sy = out.gibbs_init.data_init.y.s[1]
    
    ate_grid = linspace(-10, 10, gridN)
    
    ate_out = zeros(gridN, M)
    
    tt_out = zeros(gridN, M)
    
    late_out = zeros(gridN, M)
    
    ##hz = 0.0
    
    for i in 1:gridN
        
        for m in 1:M
            
            alpha = out.out_tuple.out_dp.alpha_out[m]
            J = out.out_tuple.out_dp.J_out[m]
            w = zeros(J + 1)
            h = zeros(J + 1)
            label = out.out_tuple.out_dp.label_out[:,m]
            njs = StatsBase.counts(label, 1:J)
            betas = out.out_tuple.out_theta.betas_out[m]
            Sigma = out.out_tuple.out_theta.Sigma_out[m]
            
            for j in 1:J
                bD = betas[1:kz,j]
                b1 = betas[kz+1:kz+kx,j]
                b0 = betas[kz+kx+1:ktot,j]
                
                xb = dot(x_new, (b1 - b0))
                zb = dot(z_new, bD)
                
                s1 = sqrt(Sigma[2,2,j])
                s0 = sqrt(Sigma[3,3,j])                
                s10 = Sigma[2,3,j]
                g2 = sqrt(s1^2 + s0^2 - 2*s10)*sy
                g1 = (Sigma[1,2,j] - Sigma[1,3,j])*(sy^2) # sig1D - sig0D
                
                ## ATE
                w[j] = pdfi( Distributions.Normal( xb, g2 ), ate_grid[i] )
                
                ## TT
                hz = ( zb + (g1/(g2^2))*(ate_grid[i] - xb) )/sqrt( 1 - (g1^2)/(g2^2) )
                hz = cdfi( Distributions.Normal(), hz )
                h[j] = w[j] + hz - cdfi( Distributions.Normal(), zb )

                ## LATE
                
            end
            
            ## draw from prior
            Sigma = NobileWishart(rho, rho*R)
            betas = rand( Distributions.MvNormal( beta_mu, beta_V ) )
            bD = betas[1:kz]
            b1 = betas[kz+1:kz+kx]
            b0 = betas[kz+kx+1:ktot]
            xb = dot(x_new, b1-b0)
            zb = dot(z_new, bD)
            g2 = sqrt(Sigma[2,2] + Sigma[3,3] - 2*Sigma[2,3])*sy
            g1 = (Sigma[1,2] - Sigma[1,3])*(sy^2)
            
            ## ATE
            w[J+1] = pdfi( Distributions.Normal( xb, g2) , ate_grid[i] )

            ## compute Polya weights and sample component
            ##w_hat = exp(w)/(alpha + N);
            ##w_hat[1:J] = njs.*w_hat[1:J]
            ##w_hat[J+1] = alpha*w_hat[J+1]
            ##ji = rand( Distributions.Categorical( w_hat/sum(w_hat) ) )
            
            ## compute ppd ATE at grid point i, for draw m
            ate_out[i,m] = ( sum(njs.*exp(w[1:J])) + alpha*exp(w[J+1]) )/(N + alpha) ##w[ji]
            
            ## TT
            hz = ( zb + (g1/(g2^2))*(ate_grid[i] - xb) )/sqrt( 1 - (g1^2)/(g2^2) )
            hz = cdfi( Distributions.Normal(), hz )
            h[J+1] = w[J+1] + hz - cdfi( Distributions.Normal(), zb )
            
            ## compute ppd TT at grid point i, for draw m
            tt_out[i,m] = ( sum(njs.*exp(h[1:J])) + alpha*exp(h[J+1]) )/(N + alpha)
            
        end
    end

    if sums
        ## 3. ppd(ate_i) = sum_m p(ate_i |theta_i) / M
        ate_out = sum(ate_out, 2)/M
        tt_out = sum(tt_out, 2)/M
        late_out = sum(late_out, 2)/M
    end
    
    return PPD(grid = ate_grid, ate = ate_out, tt = tt_out, late = late_out)
    
end

function dpmixture_ppd(out::GibbsOut, x_new::Array, z_new::Array; gridN=100)
    
    const pdfi = Distributions.pdf
    ## NB: can take argument pdf(d, a:b) where a:b is a range
    ##define: pdfi(d) = pdfi(d, ate_grid); then call: ppdi(d)

    const cdfi = Distributions.cdf
end

## function to plot ate ppd using Gadfly
function plot_ppd(ppd::PPD)
    ##Gadfly.plot(x=collect(ppd.grid), y=ppd.ate, Geom.line)
end

## ppd for ATE from blocked Gibbs sampler (or FMN)
function blocked_ate(out::GibbsOut, x_new::Array; gridN=100, log_ppd=true)

    if log_ppd
        ppdi = Distributions.logpdf
    else
        ppdi = Distributions.pdf
    end
    
    ## constants
    const M = out.out_tuple.out_M
    const N = out.gibbs_init.constant_init.dim.n
    const kx = out.gibbs_init.constant_init.dim.kx
    const kz = out.gibbs_init.constant_init.dim.kz
    const ktot = out.gibbs_init.constant_init.dim.ktot
    const J = out.gibbs_init.constant_init.prior.prior_dp.J

    ## prior parameters    
    const rho = out.gibbs_init.constant_init.prior.prior_sigma.sigma_rho
    const R = out.gibbs_init.constant_init.prior.prior_sigma.sigma_R
    const beta_mu = out.gibbs_init.constant_init.prior.prior_beta.beta_mu
    const beta_V = out.gibbs_init.constant_init.prior.prior_beta.beta_V

    ## if scaled response, sy = std(y), else sy = 1
    const sy = out.gibbs_init.data_init.y.s

    ate_grid = linspace(-10, 10, gridN)
    
    ate_out = zeros(gridN, M)

    w = zeros(J)
    w_hat = zeros(J)

    for i in 1:gridN
        
        for m in 1:M
            ## extract stick-breaking weights
            v = out.out_tuple.out_dp.eta_out[:,m]
            ## extract draw m parameters
            betas = out.out_tuple.out_theta.betas_out[m]
            Sigma = out.out_tuple.out_theta.Sigma_out[m]
            for j in 1:J
                ## compute probability weights
                w[j] = v[j]*prod(1 - v[1:j-1])
                ## compute ATE parameters
                b1 = betas[kz+1:kz+kx,j]
                b0 = betas[kz+kx+1:ktot,j]
                s1 = sqrt(Sigma[2,2,j])
                s0 = sqrt(Sigma[3,3,j])
                s10 = Sigma[2,3,j]
                xb = dot(x_new, (b1 - b0))
                g2 = sqrt(s1^2 + s0^2 - 2*s10)*sy
                ## compute p(y_new|theta_j)'s
                w_hat[j] = Distributions.logpdf( Distributions.Normal( xb, g2 ), ate_grid[i] )
            end
            ## compute ppd ATE for grid point i, draw m
            ate_out[i,m] = sum(w.*exp(w_hat))
        end
    end

    ate_out = sum(ate_out, 2)/M
    
    return PPD(grid = ate_grid, ate = ate_out)
    
end

function gaussian_ppd()

end
