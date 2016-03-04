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

## scale input data to have 0 mean and unit variance
function scale_input(A::Array)

    ## input: A (n x p)
    
    p = size(A, 2)
    
    if p > 1
        L = squeeze( std(A, 1), 1)
        L = diagm(L, 0) # p x p
        mu = mapslices(a -> a - mean(a), A, 1) # n x p
        B = L\mu' # p x n
        B = B'
    else
        L = std(A)
        mu = A - mean(A)
        B = mu/L
    end

    return B

end

## center and scale data A
function standardize(A::Array; B=2)

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
            s[p] = 1
            m[p] = 0
        end
        s[p] = std(A[:,k])
        m[p] = mean(A[:,k])
        a[:,p] = (A[:,k] - m[p])/s[p]
    end
    
    return ScaleData(a=a, m=m, s=s)
    
end


## re-scale coefficients
function scale_beta(A::Array{Float64}, my::Float64, sy::Float64, mx::Float64, sx::Float64)

    ## input
    ## A (n x p): original data
    ## betas:
    ## Sigma:
    b = zeros(size(A))
    b[1] = sy*(A[1] + my - A[2]*mx/sx)
    b[2] = A[2]*sy/sx

    return b
    
end


## re-scale coefficients, redux
function rescale_beta(beta::Array{Float64}, xs::ScaleData, ys::ScaleData)

    x = xs.a
    mx = xs.m
    sx = xs.s

    y = ys.a
    my = ys.m[1]
    sy = ys.s[1]
    
    p = size(beta, 2)

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
    
    ys = out.gibbs_init.data_init.y
    xs = out.gibbs_init.data_init.xmat
    zs = out.gibbs_init.data_init.zmat
    
    M = out.out_tuple.out_M
    kx = out.gibbs_init.constant_init.dim.kx
    kz = out.gibbs_init.constant_init.dim.kz
    ktot = 2kx + kz
    
    for m in 1:M
        J = out.out_tuple.out_dp.J_out[m]
        betas = out.out_tuple.out_theta.betas_out[m]
        for j in 1:J
            ## rescale each coefficient
            bD = rescale_beta(betas[1:kz,j], zs, ys)
            b1 = rescale_beta(betas[kz+1:kz+kx,j], xs, ys)
            b0 = rescale_beta(betas[kz+kx+1:ktot,j], xs, ys)  
            betas[:,j] = [bD, b1, b0]
        end
        out.out_tuple.out_theta.betas_out[m] = betas
    end
    
    return out
    
end
