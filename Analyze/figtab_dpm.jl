## make figs and tabs

## --------------------------------------------------------------------------- # 

## load causal mixture module (packages, functions, etc.)
if !in( "CausalMixtures", readdir() )
    error("Module not found!")
end

push!(LOAD_PATH, ".")
using CausalMixtures
using Gadfly, Mmap

## --------------------------------------------------------------------------- #

## set plot parameters
min_cdf = -10
max_cdf = 10

drop_first = false

## --------------------------------------------------------------------------- # 

## path to data?
if length(ARGS) == 0
    error("Must provide path to data")
end

## --------------------------------------------------------------------------- #

# where to save tables/figs?
if length(ARGS) == 1
    path_to_save = pwd()
else    
    path_to_save = ARGS[2]
    ##path_to_save = "../Paper/"
end

## --------------------------------------------------------------------------- #

## how many PPD samples?
if length(ARGS) == 2
    println("I don't know M_star!\nChoosing M_star = 1000")
    M_star = 1000
else
    M_star = ARGS[3] # 200040
end

## --------------------------------------------------------------------------- #

## set paths for writing 
tab_path = joinpath(path_to_save, "Tables") #"../Paper/Tables/"
fig_path = joinpath(path_to_save, "Figs") #"../Paper/Figs/"

if !( isdir(tab_path) & isdir(fig_path) )
    mkpath(tab_path)
    mkpath(fig_path)
end

## --------------------------------------------------------------------------- #

## set paths for reading
path_to_ynew = ARGS[1] # path/to/ppd/ynew
##path_to_ynew = "/home/juanfung/Documents/scpd/Analyze/Data/blocked/ppd/ynewCtop2000champaign.bin"

mmap_path, ynewBin = splitdir(path_to_ynew) # (path/to/ppd, ynew)
path_to_data, ppd = splitdir(mmap_path) # (path/to, ppd)
##mmap_path = joinpath(path_to_data, "ppd")
f = replace(ynewBin, r"^ynew(.*)\.bin$", s"\1")

## --------------------------------------------------------------------------- # 

## load input data
@time state = JLD.jldopen( joinpath(path_to_data, "state.jld"), "r");
@time input = read(state, "input");
close(state)
state = 0

## load PPD draws
s = open( path_to_ynew, "r")
offsi = 0
ynew = Mmap.mmap(s, Matrix{Float64}, (3, M_star), offsi)
close(s)

if drop_first
    ynew = ynew[2:end]
end

s = 0

## --------------------------------------------------------------------------- #

## compute Treatment Effects
@time te = CausalMixtures.dpm_ate(ynew, input);
 
## compute summary stats    
StatsBase.summarystats(te.ate)

StatsBase.summarystats(te.tt)

## --------------------------------------------------------------------------- # 

## set file names
fignames = tabname = input.params.model * f


## --------------------------------------------------------------------------- #

## signature:
##function make_table(ate, tt;
##                    tab_name="tab", tab_path=pwd(), y=0.0, output = true)

## --------------------------------------------------------------------------- #

## create tables
@time df_dpm = CausalMixtures.make_table(ate, tt,
                                         tab_name = tabname * "_logs",
                                         tab_path = tab_path,
                                         y=0.0)

## exponentiated:
@time df_dpm = make_table(exp(ate)-1, exp(tt)-1,
                          tab_name = tabname,
                          tab_path = tab_path,
                          y=0.0)

## --------------------------------------------------------------------------- #

## create figs
Gadfly.plot( layer(x=te.ate, Geom.density, Theme(default_color=colorant"red")),
             layer(x=te.tt, Geom.density, Theme(default_color=colorant"blue")),
             Guide.manual_color_key("Legend", ["ATE", "TT"], ["red", "blue"]))
