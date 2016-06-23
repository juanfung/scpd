## run as:
## nohup nice julia -p 5 --precompiled=yes parpost_dpm.jl &> parpost.log & tail -f parpost.log

## --------------------------------------------------------------------------- #

@everywhere push!(LOAD_PATH, ".")
using CausalMixtures

@everywhere include("load-paths.jl")

@time ynew = CausalMixtures.parallel_rand_ppd(path_to_data, path_to_znew);

## --------------------------------------------------------------------------- #

println("Saving draws...")

mmap_path = joinpath(path_to_data, "ppd")

if !isdir(mmap_path)
    mkpath(mmap_path)
end

println("M_star = $(length(ynew))")

ynewBin = open( joinpath(mmap_path, "ynew" * f * ".bin"), "a+" )

write(ynewBin, ynew)

close(ynewBin)

println("Done!")

## --------------------------------------------------------------------------- # 
