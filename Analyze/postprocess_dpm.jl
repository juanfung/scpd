## process MCMC output -> ppd draws

## nohup nice julia --precompiled=yes postprocess_dpm.jl "/home/juanfung/Documents/scpd/Analyze/Data/gaussian/" "./tmp/zvec2000champaign.dat" &> output.log & tail -f output.log

## --------------------------------------------------------------------------- # 

if length(ARGS) == 0
    error("Must provide path to data and predictors")
end

path_to_data = ARGS[1]
path_to_znew = ARGS[2]

buyer = replace(path_to_znew, r".*zvec\d{4}(.*)\.dat$", s"\1")
year = replace(path_to_znew, r".*zvec(\d{4}).*\.dat$", s"\1")

write_ppd = false

## --------------------------------------------------------------------------- # 
## load causal mixture module (packages, functions, etc.)

if !in( "CausalMixtures", readdir() )
    error("Module not found!")
end

push!(LOAD_PATH, ".")
using CausalMixtures

## --------------------------------------------------------------------------- # 
## 1. load data and predictors

@printf("Loading state and input...\n")
@time state = JLD.jldopen( joinpath(path_to_data, "state.jld"), "r");

## load input data
@time input = read(state, "input");
close(state)
state = 0

@printf("Loading predictors...\n")
@time znew = readcsv(path_to_znew); # kz x 2

## --------------------------------------------------------------------------- #
## 2. open connections for writing

## set path to save, create if necessary
mmap_path = joinpath(path_to_data, "ppd")

if !isdir(mmap_path)
    mkpath(mmap_path)
end

f = year * buyer * ".bin"

## save champaignhome
yNewC = open( joinpath(mmap_path, "yNewC" * f), "a+")
ateNewC = open( joinpath(mmap_path, "ateNewC"  * f), "a+")
ttNewC = open( joinpath(mmap_path, "ttNewC"  * f), "a+")

## save urbanahome
yNewU = open( joinpath(mmap_path, "yNewU"  * f), "a+")
ateNewU = open( joinpath(mmap_path, "ateNewU"  * f), "a+")
ttNewU = open( joinpath(mmap_path, "ttNewU"  * f), "a+")

## --------------------------------------------------------------------------- # 
## 3. generate PPD draws, compute TEs

## get output names...
outs = readdir( joinpath(path_to_data, "output") )

if length(outs) == 0
    error("Empty directory!")
end

## pre-allocate offset storage...
M_star = Array(Int64, length(outs));
if write_ppd
    M_treats = Array(Int64, length(outs), 4)
    offs = Array(Int64, length(outs), 6);
end

## sort outputs...
order = [parse(Int, replace(o, r"out-(\d+)-\d+.*$", s"\1")) for o in outs]
outs = [outs order]
outs = sortrows(outs, by=x->x[2])

@printf("Generating predictions for %s model...\n", input.params.model)
for i in 1:size(outs,1)
    o = outs[i]
    println("Loading $(o)...")
    @time o = JLD.jldopen(path_to_data * "output/" * o, "r");
    @time out = read(o, "out");
    close(o)
    M_star[i] = length(out.out_data) # NB: only useful for size(ynew, 2)
    ###########################
    ### Sample for Champaign ##
    ###########################
    println("Sampling from posterior predictive: Champaign home...")
    @time ynew = CausalMixtures.rand_ppd(out, input, znew[:,1]);
    if write_ppd
        ## write ppd...
        offs[i, 1] = write(yNewC, ynew)
        println("Computing treatment effects...")
        treats = CausalMixtures.dpm_ate(ynew, input);
        ## write ate...
        println("mean ate = $(mean(treats.ate))")
        M_treats[i, 1] = length(treats.ate)
        offs[i, 2] = write(ateNewC, treats.ate)
        ## write tt...
        println("mean tt = $(mean(treats.tt))")
        M_treats[i, 2] = length(treats.tt)
        offs[i, 3] = write(ttNewC, treats.tt)
        ###########################
        ### Sample for Urbana #####
        ###########################
        println("Sampling from posterior predictive: Urbana home...")
        @time ynew = CausalMixtures.rand_ppd(out, input, znew[:,2]);
        ## write ppd...
        offs[i, 1] = write(yNewU, ynew)
        println("Computing treatment effects...")
        treats = CausalMixtures.dpm_ate(ynew, input);
        ## write ate...
        println("mean ate = $(mean(treats.ate))")
        M_treats[i, 3] = length(treats.ate)
        offs[i, 2] = write(ateNewU, treats.ate)
        ## write tt...
        println("mean tt = $(mean(treats.tt))")
        M_treats[i, 4] = length(treats.tt)
        offs[i, 3] = write(ttNewU, treats.tt)
    end    
    ##################
    ## Clear memory ##
    ##################
    o = out = 0
    println("Done!")
end

##last_out = sum(M_star) - outs[1,2] + 1
last_out = replace(outs[end,1], r"out-\d+-(\d+).*$", s"\1")

f = "$(outs[1,2])-$(last_out)"

s = open( joinpath(mmap_path, "Mstar-" * f * ".dat"), "a+")
write(s, M_star)
close(s)

if write_ppd
    ## save counts
    s = open( joinpath(mmap_path, "Mtreats-" * f * ".dat"), "a+")
    write(s, M_treats)
    close(s)
    ## save offsets
    s = open( joinpath(mmap_path, "offs-" * f * ".dat"), "a+")
    write(s, offs)
    close(s)
end

## --------------------------------------------------------------------------- # 
## 4. close connections

close(yNewC)
close(ateNewC)
close(ttNewC)

close(yNewU)
close(ateNewU)
close(ttNewU)

@printf("Iterations %s: Done!", f)

## --------------------------------------------------------------------------- #
