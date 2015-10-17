## Posterior predictive densities for DP Mixture

type PosteriorPredictive
    grid::LinSpace{Float64}
    ate::Array{Float64}
end

typealias PPD PosteriorPredictive

PPD(;grid=linspace(-2,2,10), ate=zeros(10)) = PPD(grid, ate)

## PPD for ATE_i at X = x, where i is in the Data
## 1. for each m, get i's label and use corresponding parameters
##    - essentially the output PPD

## PPD for ATE_new at X = x, for new observation
function dpmixture_ate(out::GibbsOut, x::Array; N=100)

    ## 1. construct grid of N points {ate = y1 - y0}
    ##   - where y1 - y0 ~ N( (b1 - b0)'x, g2)
    
    M = out.out_tuple.out_M

    ate_grid = linspace(-10, 10, N)
    
    ate_out = zeros(N, M)
    
    ## Then
    ## for i in 1:N
    
    ## 2. for each m in 1:M
    for m in 1:M
        ##   -  Sample theta_i (ie, label for)
        ##     -  where pr(label_i = new) = alpha[m] * F(ate_i | Theta_0)
        ##     -  and   pr(label_i = j) = nj * F(ate_i | Theta[j,m]), j = 1,...,J[m]
        ##   - compute p(ate_i | theta_i) = N( (b1_i - b0_i)'x, g2_i)
        ##     -  where g2 = s1^2 + s2^2 - 2*s10
    end
    

    ## 3. ppd(ate_i) = sum_m p(ate_i |theta_i) / M

    return PPD(grid = ate_grid, ate = ate_out)
    
end

