## pre-process raw data for simulations

## --------------------------------------------------------------------------- #

## load packages
##using JLD, HDF5

push!(LOAD_PATH, ".")
using CausalMixtures # imports DataFrames, JLD, HDF5

## --------------------------------------------------------------------------- #

path_to_raw = "./Data/"

name_of_raw = "df_1.csv"

## --------------------------------------------------------------------------- #

println("Loading raw data...")

## REMARKS:
## 1a. data should be limited to 1996-2001 (or mid 2002)... (decrease n)
## 1b. drop very low and very high Sale.Price
## 2. add dummies (or school year) dummies, tract dummies... (increase k)
## 3. revisit xvars, zvars...
## 4. learning alpha? more diffuse prior?
## 5. hyperpriors on theta?

## --------------------------------------------------------------------------- # 

## Parallelize across workers:
## 1. wrap this file as
##function run(i::Int64)
##    df_path = "./Data/df_"*string(i)*".csv"
##    ...
##end
## 2. Then:
## out = mmap(run, [1:10])

## --------------------------------------------------------------------------- # 

df = DataFrames.readtable(path_to_raw * name_of_raw);

println("Processing data...")

## pool factors
facs = [:Garage, :Agency_Name, :Post_Choice, :ProxB, :Buyer_Origin, :Year, :TRACTCE00]
DataFrames.pool!(df, facs)

## get outcome variables
xvars = DataFrames.readtable("./Data/xvars.csv")
xvars = Array(xvars[:x_1])
xvars = map(s -> replace(s, ".", "_"), xvars)

## get selection variables
zvars = readdlm("./Data/zvars.csv", ',', AbstractString)
zvars = zvars[2:end,2]
zvars = map(s -> replace(s, ".", "_"), zvars)

## get treatment choice
d_obs = df[:d_obs];

## get observed outcome
y_obs = df[:Log_Sale_Price];

## get models
form_rhs = ""
for i in 1:size(xvars, 1)
    form_rhs = join([form_rhs; xvars[i]], " + " )
end
form_rhs = replace(form_rhs, r"^\s\+\s", "" )

y_form = DataFrames.Formula(:Log_Sale_Price, parse(form_rhs) )

form_rhs = join([form_rhs; zvars], " + " )

d_form = DataFrames.Formula(:d_obs, parse(form_rhs) )

##mm_d = DataFrames.ModelFrame(d_form, df)
##mm_d = DataFrames.ModelMatrix( mm_d )

mm_d = DataFrames.ModelMatrix( DataFrames.ModelFrame(d_form, df) ).m;

mm_y = DataFrames.ModelMatrix( DataFrames.ModelFrame(y_form, df) ).m;

kx = size(mm_y, 2)
kz = size(mm_d, 2)
ktot = 2kx + kz

## --------------------------------------------------------------------------- #
## write to file for re-loading

println("Saving processed data...")

@time JLD.jldopen(path_to_raw * "df.jld", "w", compress=true) do file
    JLD.addrequire(file, DataFrames)
    write(file, "df", df)
    write(file, "y_form", y_form)
    write(file, "d_form", d_form)
    g = HDF5.g_create(file, "dims")
    g["kx"] = kx
    g["kz"] = kz
    g["ktot"] = ktot
end;

println("Done!")

## --------------------------------------------------------------------------- #
