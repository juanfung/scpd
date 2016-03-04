## Posterior predictive densities for DP Mixture

type PosteriorPredictive
    grid::LinSpace{Float64}
    ate::Array{Float64}
end

typealias PPD PosteriorPredictive

PPD(;grid=linspace(-2,2,10), ate=zeros(10)) = PPD(grid, ate)

## PPD for ATE_i at X = x, where i is in the Data
## 1. for each m, get i's label and use corresponding parameters
##    - essentially the Gibbs output PPD

## PPD for ATE_new at X = x_new, for new observation
function dpmixture_ate(out::GibbsOut, x_new::Array; gridN=100, log_ppd=true)
    
    ## 1. construct grid of gridN points {ate = y1 - y0}
    ##   - where y1 - y0 ~ N( (b1 - b0)'x_new, g2)
    
    if log_ppd
        const ppdi = Distributions.logpdf
    else
        const ppdi = Distributions.pdf
    end
    
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
    const sy = out.gibbs_init.data_init.y.s
    
    ate_grid = linspace(-10, 10, gridN)
    
    ate_out = zeros(gridN, M)
    
    ## Then
    ## 1. for each grid point i:
    
    for i in 1:gridN
        
        ## 2. for each MCMC draw m:
        
        for m in 1:M
            
            ##   -  Sample theta_i (ie, label for i)
            ##     -  where pr(theta_i = theta_new) = alpha[m] * F(ate_i | Theta_0)/(alpha[m]+N)
            ##     -  and   pr(theta_i = theta_j) = nj * F(ate_i | Theta[j,m])/(alpha[m]+N),
            ##        for j = 1,...,J[m]
            ##   - compute p(ate_i | theta_i) = N( (b1_i - b0_i)'x_new, g2_i)
            ##     -  where g2 = s1^2 + s2^2 - 2*s10
            
            alpha = out.out_tuple.out_dp.alpha_out[m]
            J = out.out_tuple.out_dp.J_out[m]
            w = zeros(J + 1)
            label = out.out_tuple.out_dp.label_out[:,m]
            njs = StatsBase.counts(label, 1:J)
            betas = out.out_tuple.out_theta.betas_out[m]
            Sigma = out.out_tuple.out_theta.Sigma_out[m]
            
            for j in 1:J
                b1 = betas[kz+1:kz+kx,j]
                b0 = betas[kz+kx+1:ktot,j]
                s1 = sqrt(Sigma[2,2,j])
                s0 = sqrt(Sigma[3,3,j])
                s10 = Sigma[2,3,j]
                xb = dot(x_new, (b1 - b0))
                g2 = sqrt(s1^2 + s0^2 - 2*s10)*sy
                w[j] = ppdi( Distributions.Normal( xb, g2 ), ate_grid[i] )
            end
            
            ## draw from prior
            Sigma = NobileWishart(rho, rho*R)
            betas = rand( Distributions.MvNormal( beta_mu, beta_V ) )
            b1 = betas[kz+1:kz+kx,j]
            b0 = betas[kz+kx+1:ktot,j]
            xb = dot(x_new, b1-b0)
            g2 = sqrt(Sigma[2,2] + Sigma[3,3] - 2*Sigma[2,3])*sy
            w[J+1] = ppdi( Distributions.Normal( xb, g2) , ate_grid[i] )
            
            ## compute Polya weights and sample component
            ##w_hat = exp(w)/(alpha + N);
            ##w_hat[1:J] = njs.*w_hat[1:J]
            ##w_hat[J+1] = alpha*w_hat[J+1]
            ##ji = rand( Distributions.Categorical( w_hat/sum(w_hat) ) )
            
            ## compute ppd ATE at grid point i, for draw m
            ate_out[i,m] = ( sum(njs.*exp(w[1:J])) + alpha*exp(w[J+1]) )/(n + alpha) ##w[ji]
        end
    end
    
    ## 3. ppd(ate_i) = sum_m p(ate_i |theta_i) / M
    ate_out = sum(ate_out, 2)/M
    
    return PPD(grid = ate_grid, ate = ate_out)
    
end

## function to plot ate ppd using Gadfly
function plot_ate(ppd::PPD)
    ##Gadfly.plot(x=collect(ppd.grid), y=ppd.ate, Geom.line)
end

## ppd for ATE from blocked Gibbs sampler
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


## choose ATE, TTE, ITT, or LATE
function dpmixture_ppd()
    
end
