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
function dpmixture_ate(out::GibbsOut, x_new::Array; N=100, log_ppd=true)

    ## 1. construct grid of N points {ate = y1 - y0}
    ##   - where y1 - y0 ~ N( (b1 - b0)'x_new, g2)

    ## constants
    M = out.out_tuple.out_M
    N_data = out.gibbs_init.constant_init.dim.n
    kx = out.gibbs_init.constant_init.dim.kx
    kz = out.gibbs_init.constant_init.dim.kz
    ktot = out.gibbs_init.constant_init.dim.ktot

    ## prior parameters    
    rho = out.gibbs_init.constant_init.prior.prior_sigma.sigma_rho
    R = out.gibbs_init.constant_init.prior.prior_sigma.sigma_R
    beta_mu = out.gibbs_init.constant_init.prior.prior_beta.beta_mu
    beta_V = out.gibbs_init.constant_init.prior.prior_beta.beta_V

    ate_grid = linspace(-10, 10, N)
    
    ate_out = zeros(N, M)
    
    ## Then
    ## 1. for each grid point i:
    
    for i in 1:N
        
        ## 2. for each MCMC draw m:
        
        for m in 1:M
            
            ##   -  Sample theta_i (ie, label for i)
            ##     -  where pr(label_i = new) = alpha[m] * F(ate_i | Theta_0)
            ##     -  and   pr(label_i = j) = nj * F(ate_i | Theta[j,m]), j = 1,...,J[m]
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
                s1 = Sigma[2,2,j]
                s0 = Sigma[3,3,j]
                s10 = Sigma[2,3,j]
                xb = dot(x_new, (b1 - b0))
                g2 = s1^2 + s0^2 - 2*s10
                w[j] = Distributions.logpdf( Distributions.Normal( xb, g2 ), ate_grid[i] )
            end
            ## draw from prior
            Sigma = NobileWishart(rho, R)
            betas = rand( Distributions.MvNormal( beta_mu, beta_V ) )
            b = betas[kz+1:kz+kx] - betas[kz+kx+1:ktot]
            g2 = Sigma[2,2] + Sigma[3,3] - 2*Sigma[2,3]
            w[J+1] = Distributions.logpdf( Distributions.Normal( dot(x_new, b), g2) , ate_grid[i] )
            ## compute Polya weights
            w_hat = w/(alpha + n -1);
            w_hat[1:J] = njs.*w_hat[1:J]
            w_hat[J+1] = alpha*w_hat[J+1]
            ji = rand( Distributions.Categorical( exp(w_hat)/sum(exp(w_hat)) ) )
            ate_out[i,m] = w[ji]            
        end
    end
    
    ## 3. ppd(ate_i) = sum_m p(ate_i |theta_i) / M
    if log_ppd
        ate_out = sum(ate_out, 2)/M
        ##ate_out = mean(ate_out, 2)
    else
        ate_out = sum(exp(ate_out), 2)/M
        ##ate_out = mean(exp(ate_out), 2)
    end
    
    return PPD(grid = ate_grid, ate = ate_out)
    
end

