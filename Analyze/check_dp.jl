## check DPM output

## --------------------------------------------------------------------------- #
## re-load
##gibbs_out = load("./Data/out.jld", "gibbs_out")

## --------------------------------------------------------------------------- #
## get true values
d_true = Array(df[:d_obs])
y_true = Array(df[:Log_Sale_Price])
my = mean(y_true)
sy = std(y_true)

## compare scaled y
y_true_scaled = (y_true - my)/sy;

## --------------------------------------------------------------------------- #
## check data augmentation
y_out = gibbs_out.out_tuple.out_data.y_out;
y_out_hat = sy*mean(y_out, 2) + my;
mean(y_out_hat)

## --------------------------------------------------------------------------- #
## check ATEs
ymiss_out = gibbs_out.out_tuple.out_data.ymiss_out;
y1_out = d_true.*y_true_scaled + (1 - d_true).*mean(ymiss_out, 2);
y0_out = d_true.*mean(ymiss_out, 2) + (1 - d_true).*y_true_scaled;

ate_out = (y1_out - y0_out)*sy;
mean(ate_out)
Gadfly.plot(x=ate_out, Geom.density)

sum(ate_out.*d_true)/sum(d_true)
sum(ate_out.*(1-d_true))/sum((1-d_true))

## --------------------------------------------------------------------------- #

## evaluate at mean x values
xvec = mean(gibbs_out.gibbs_init.data_init.xmat, 1)

## ate PPD:

## evaluate at mean z values
zvec = mean(gibbs_out.gibbs_init.data_init.zmat, 1)

## tt/tut PPD:


## late PPD:


## --------------------------------------------------------------------------- #
