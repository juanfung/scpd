## run as
## (serial): 
## nohup nice julia --precompiled=yes parpost_dpm.jl &> parpost.log & tail -f parpost.log
## (parallel):
## nohup nice julia -p 4 --precompiled=yes parpost_dpm.jl &> parpost.log & tail -f parpost.log 

@everywhere test_o = "./tmp/test/"
@everywhere test_z = "./tmp/test/zvecA2016test.dat"

## --------------------------------------------------------------------------- #

@everywhere push!(LOAD_PATH, ".")
using CausalMixtures

@everywhere include("load-paths.jl")
println("Sampling ppd for zvec" * f)

## --------------------------------------------------------------------------- #
## JIT run?
@time ynew = CausalMixtures.parallel_rand_ppd(test_o, test_z);

## --------------------------------------------------------------------------- #

@time ynew = CausalMixtures.parallel_rand_ppd(path_to_data, path_to_znew);

## --------------------------------------------------------------------------- #

println("Saving draws...")

mmap_path = joinpath(path_to_data, "ppd")

if !isdir(mmap_path)
    mkpath(mmap_path)
end

println("M_star = $(size(ynew, 2))")

ynewBin = open( joinpath(mmap_path, "ynew" * f * ".bin"), "a+" )

write(ynewBin, ynew)

close(ynewBin)

println("Done!")

## --------------------------------------------------------------------------- # 
