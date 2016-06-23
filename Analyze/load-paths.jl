## set paths to load on all workers

path_to_data = "./tmp/testblocked/" ## "/home/juanfung/Documents/scpd/Analyze/Data/blocked/"

path_to_znew = "./tmp/test/zvecA2016test.dat" ## "./tmp/zvec/zvecC2000champaign.dat"

##f = replace(path_to_znew, r".*zvec(\d{4})(.*)\.dat$", s"\1\2") * ".bin"
f = replace(path_to_znew, r".*zvec(.*)\.dat$", s"\1") 

## --------------------------------------------------------------------------- # 
