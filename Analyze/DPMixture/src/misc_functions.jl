## misc functions

## flip rows 1,3 and columns 1,3 for 3x3 matrix
function flip_mat(A::Array) flipdim(flipdim(A, 1), 2) end


## sample 3x3 A ~ IW(nu, V) with constraint A(1,1)=1
function NobileWishart(nu::Int64, V::Array{Float64}; n=1)
    
    ## lower triangular Cholesky factor for V^-1
    I3 = eye(3)
    L = chol(flip_mat(V)\I3, Val{:L})

    ## initialize lower triangular A
    A = zeros(3, 3, n)
    A[2,1,:], A[3,1,:], A[3,2,:] = randn(3*n)

    ## fill in diagonal
    A[1,1,:] = sqrt( rand( Chisq(nu), n) )
    A[2,2,:] = sqrt( rand( Chisq(nu-1), n) )
    A[3,3,:] = 1/L[3,3]

    ## construct out = (L^-1)'(A^-1)'(A^-1)(L^-1)
    L = L\I3
    A = mapslices(a->a\I3, A, [1, 2])
    out = mapslices(a->*(a, L), A, [1, 2])
    out = mapslices(a->*(a', a), out, [1, 2])
    
    out = mapslices(flip_mat, out, [1, 2])

    if n == 1 out = out[:,:,1] end

    return out

end


## sample truncated normals at 0, given d = {0, 1}
function truncnorm(mu::Array, sigma::Array, d::Array)
    
    u = rand(size(mu,1))

    c = cdf(Normal(), -mu./sigma)

    p = ( c.*(1 - d) + (1 - c).*d ).*u + c.*d

    z = mu + sigma.*quantile(Normal(), p)

    return z
    
end


## center and scale continuous data, A
function standardize(A::Array{Float64}; B=2)

    ## inputs
    ## A (n x p) : data matrix
    ## B (Integer): bound on unique values for factors
    
    ## outputs
    ## a (n x p): scaled data matrix
    ## m (p x 1): centering for each column of A
    ## s (p x ): scaling for each column of A
    
    p = size(A, 2)
    s = zeros(p)
    m = zeros(p)
    a = zeros(size(A))
    
    for k in 1:p
        if size(unique(A[:,k]), 1) <= B ##eltype(A[:,k]) <: Integer
            ##next
            s[k] = 1.0
            m[k] = 0.0            
        else  
            s[k] = std(A[:,k])
            m[k] = mean(A[:,k])            
        end
        a[:,k] = (A[:,k] - m[k])/s[k]
    end
    
    return ScaleData(a=a, m=m, s=s)
    
end


## re-scale coefficients, redux
function rescale_beta(beta::Array{Float64}, xs::ScaleData, ys::ScaleData)

    x = xs.a
    mx = xs.m
    sx = xs.s

    y = ys.a
    my = ys.m[1]
    sy = ys.s[1]
    
    p = size(beta, 1)
    
    b = zeros(size(beta))
    
    ##b[1] = sy*( beta[1] + my - sum(beta[2:p].*mx[2:p]./sx[2:p]) ) # NB: mx[1]=0
    b[1] = sy*( beta[1] + my - dot( beta, mx./sx ) )
    for k in 2:p
        b[k] = beta[k]*sy/sx[k]
    end    
    
    return b
    
end


## rescale MCMC output
function rescale_output(out::GibbsOut)
    
    const ys = out.gibbs_init.data_init.y    
    const xs = out.gibbs_init.data_init.xmat
    const zs = out.gibbs_init.data_init.zmat
    
    const M = out.out_tuple.out_M    
    const kx = out.gibbs_init.constant_init.dim.kx
    const kz = out.gibbs_init.constant_init.dim.kz
    const ktot = 2kx + kz

    const init = out.gibbs_init
    const out_data = out.out_tuple.out_data
    const out_dp = out.out_tuple.out_dp    
    out_theta = out.out_tuple.out_theta
    
    ## scale by sy
    sy = ys.s[1]
    sy = diagm([1.0, sy, sy])
    
    for m in 1:M
        J = out.out_tuple.out_dp.J_out[m]
        betas = out_theta.betas_out[m]
        Sigma = out_theta.Sigma_out[m]
        for j in 1:J
            ## rescale each coefficient
            bD = rescale_beta(betas[1:kz,j], zs, ys)
            b1 = rescale_beta(betas[kz+1:kz+kx,j], xs, ys)
            b0 = rescale_beta(betas[kz+kx+1:ktot,j], xs, ys)  
            betas[:,j] = vcat(bD, b1, b0)
            Sigma[:,:,j] = sy*Sigma[:,:,j]*sy
        end
        out_theta.betas_out[m] = betas
        out_theta.Sigma_out[m] = Sigma
    end
    
    ##return out
    return GibbsOut(out_tuple = OutTuple(out_M=M, out_data=out_data, out_dp=out_dp, out_theta=out_theta),
                    gibbs_init = init)
    
end


## sample DP concentration parameter a la Escobar and West (1994)
## TODO: change signature to (dp_state::StateDP, dp_prior::PriorDP; n::Int64=100)
function sample_alpha(alpha::Float64, J::Int64; n::Int64=100, shape::Float64=1.0, rate::Float64=1.0)

    ## sample auxiliary variable
    eta = rand( Distributions.Beta(alpha+1, n) )

    ## update shape and rate parameters
    b_star = rate - log(eta)
    A = (shape + J - 1)/(n*b_star)
    pi_eta = A/(1 + A)
    a_star = ifelse(pi_eta > 0.5, shape + J, shape + J - 1)
    
    ## sample alpha
    alpha = rand( Distributions.Gamma(a_star, 1/b_star) )

    return alpha, eta

end

