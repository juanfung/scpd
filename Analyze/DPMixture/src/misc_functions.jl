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

function scale_coef(A::Array, s::Array)
    
end

## estimate PPD for ATE
function ppd_ate(out::OutTuple)

    betas_out = out.out_theta.betas_out
    Sigma_out = out.out_theta.Sigma_out

    J_out = out.out_dp.J_out
    
    return

end     
