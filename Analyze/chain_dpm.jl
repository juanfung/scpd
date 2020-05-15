## --------------------------------------------------------------------------- #
##nohup nice julia --precompiled=yes chain_dpm.jl "/home/juanfung/Documents/scpd/Analyze/Data/fmn/" 5000 &> fmn.log &

if length(ARGS) == 0
    error("Must provide path to data\nOptional: number of iterations")
end

if length(ARGS) == 1
    M = 1000
else
    M = parse(Int, ARGS[2])
end

## --------------------------------------------------------------------------- #

## load causal mixture module (packages, functions, etc.)
push!(LOAD_PATH, ".")
using CausalMixtures

##path_to_data = "/home/juanfung/Documents/scpd/Analyze/Data/dpm/"
##path_to_data = "/home/juanfung/Documents/scpd/Analyze/Data/" * ARGS[1] * "/"
path_to_data = ARGS[1]

compress_out = true

## --------------------------------------------------------------------------- #
## init JIT
@printf("Initializing JLD...\n")
@time JLD.jldopen("./tmp/test.jld", "r+", compress=true) do file
    delete!(file, "M")
    write(file, "M", M)
end;

## --------------------------------------------------------------------------- #
## reload and continue chain...
@printf("Loading state and input...\n")
@time out = JLD.jldopen(path_to_data * "state.jld", "r");
@time state = read(out, "state");
@time input = read(out, "input");
close(out)

@printf("Initializing sampler...\n")
input.params.M = 1
@time state, input, out = CausalMixtures.dpm!(state, input);

input.params.M = M
@time state, input, out = CausalMixtures.dpm!(state, input, out); 
##@time state, input, out = CausalMixtures.dpm!(state, input); ## OOPS! - forgot to store first draw!

@printf("Saving state...\n")
@time JLD.jldopen(path_to_data * "state.jld", "r+", compress=compress_out) do file
    delete!(file, "state")
    write(file, "state", state) # save state
end;

@printf("Saving output...\n")
out_1 = state.state_sampler.batch_m - input.params.M

fname = "output/out-$(out_1)-$(state.state_sampler.batch_m).jld"
@time JLD.jldopen(path_to_data * fname, "w", compress=compress_out) do file
    JLD.addrequire(file, CausalMixtures)
    write(file, "out", out)
end;

##fname = "out-$(out_1)-$(state.state_sampler.batch_m).jld"
##@time JLD.jldopen(path_to_data * "out.jld", "r+", compress=compress_out) do file
##    write(file, fname, out)
##end;

@printf("Next %d (of %d) iterations complete and saved!",
        input.params.M + 1, state.state_sampler.batch_m)

## --------------------------------------------------------------------------- #
