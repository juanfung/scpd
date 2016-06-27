## In case you lose the current state...
## use output to reset state to a given m
## NB: requires old state to copy state.state_sampler

## --------------------------------------------------------------------------- #
push!(LOAD_PATH, ".")
using CausalMixtures

## --------------------------------------------------------------------------- #
path_to_data = ARGS[1] # where is the data
o = AGRS[2] # desired output
##s = ARGS[3] # old state

compress_out = true

## --------------------------------------------------------------------------- #
println("Loading old state...")
path_to_state = joinpath(path_to_data, "state.jld")
##path_to_state = joinpath(path_to_data, s)
@time ss = JLD.jldopen( path_to_state, "r");
@time state = read(ss, "state");
close(ss)

## --------------------------------------------------------------------------- #
println("Loading output...")
@time oo = JLD.jldopen( joinpath( path_to_data, "output/" * o ), "r");
@time out = read(oo, "out");
close(oo)

## --------------------------------------------------------------------------- #
println("Updating state...")
@time state = CausalMixtures.reset_state!( out, state, length(out.out_dp) );
m = parse(Int, replace(o, r"^out-\d+-(\d+).jld$", s"\1"))
state.state_sampler.batch_m = m

## --------------------------------------------------------------------------- #
println("Saving updated state...")
@time JLD.jldopen(path_to_state, "r+", compress=compress_out) do file
    delete!(file, "state")
    write(file, "state", state) # save state
end;

## --------------------------------------------------------------------------- #
println("Done!")

## --------------------------------------------------------------------------- #
