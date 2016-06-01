## fit hedonic diff-in-diff

library(xtable)
#options(xtable.floating = FALSE)
options(xtable.timestamp = "")

path_to_raw = "./Data"
name_of_df = "df_1.csv"

path_to_save = "../Paper/Tables"

df = read.csv( paste(path_to_raw, name_of_df, sep="/"), quote="\"", skipNul = TRUE);

xvars = read.csv( paste(path_to_raw, "xvars.csv", sep="/"), quote="\"", skipNul = TRUE, stringsAsFactors=FALSE);
(xvars = xvars["x"][,1])

zvars = read.csv( paste(path_to_raw, "zvars.csv", sep="/"), quote="\"", skipNul = TRUE, stringsAsFactors=FALSE);
(zvars = zvars["x"][[1]])

y.obs = "Log.Sale.Price"
d.obs = "d.obs"

## 2. set models

#( d.form = as.formula(paste(" ~ ", paste(c(xvars, zvars), collapse="+"))) )
( y.form = as.formula(paste(y.obs, " ~ ", paste(c(xvars, zvars,d.obs), collapse="+"))) )

##as.int.fac = function(f) {as.integer(levels(f))[f]}

## set factors
facs <- c("Garage", "Agency.Name", "Post.Choice", "ProxB", "Buyer.Origin", "Year", "TRACTCE00")

for (f in facs) {
    df[,f] <- factor(df[,f])
}

## fit model
ddh = lm(y.form, df)

## output results to .tex file
(ddh.tab = xtable(ddh,
                  caption="Estimates for DD hedonic model",
                  latex.environments = "center",
                  label="tab:dd") )

sink( paste(path_to_save, "dd-out.tex", sep="/") )

print(ddh.tab, booktabs=TRUE,
      sanitize.text.function = function(x) gsub("([a-z])(\\.|)([A-Z0-9])", "\\1 \\3", x) )

sink()

##ddh.out = toLatex(ddh.tab)

autoformat(ddh.tab)

## Alternative:
##library(lme4)
##y.form  = as.formula("Log.Sale.Price ~ xvars + (1 | Year) + (1 | Agency.Name)")
## or:
##x + (x | g) - correlated random intercept and slope
##x + (x || g) - uncorrelated random intercept and slope

## ddh = lmer(y.form, df)
