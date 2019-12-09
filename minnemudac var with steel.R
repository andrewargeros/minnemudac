### THIS IS AN R SCRIPT USED FOR MINNEMUDAC 2019 FOR THE PREDICTION OF END-OF-DAY [EOD] SOYBEAN FUTURES PRICES 
### HAMLINE UNIVERSITY | HAMLINE SCHOOL OF BUSINESS 2019 -- ANDREW ARGEROS, DJ BROWN, LINDSAY HAWK, LINDSAY STEIGER
### CREATED 10/30/2019 & FINALIZED 11/03/2019 FOR USE AT MINNEMUDAC DATA ANALYTICS UNDERGRADUATE COMPETITION - 11/09/2019

# these are the the required packages to execute this package:
require(lubridate)
require(DataCombine) 
require(vars) 
require(prophet) 
require(forecast)
require(dygraphs)
require(dplyr) 
require(tsDyn)
require(aTSA) 
require(ggplot2)

mudac = read.csv("marchwsteelWEEKENDS.csv", header = TRUE) # change file name for each contract `.csv` file 
tail(mudac) 
mudac$Date = mudac[,1]

fcast.pers = 1 # this is a trimming value [set to 1]
proph.var = c()
for(i in 2:7){ # this is a for-loop to turn the other commodity data into prophet forcasts
  date = mudac$Date
  y.m.d = format(as.Date(date, "%m/%d/%Y"), '%Y-%m-%d') # prophet requires ds to be %Y-%m-%d instead of mdy
  mudac$ymd = y.m.d
  varns = variable.names(mudac)[i]
  mudac.p = mudac[,c("ymd",varns)]
  mudac.p
  mudac.p$ds = y.m.d 
  mudac.p$y = mudac.p[,2] 
  d = mudac.p[,c("ds","y")] # prophet requires names "ds" for datestamp and "y" for time series variable
  pro = prophet(d, weekly.seasonality = TRUE, yearly.seasonality = TRUE, daily.seasonality = TRUE)
  future = make_future_dataframe(pro, periods = fcast.pers)
  forecast = predict(pro, future) #creates predictions
  proph.var = cbind(proph.var, forecast$yhat)
}


proph.var = as.data.frame(proph.var)
proph.var = proph.var[1:(dim(proph.var)[1]-fcast.pers),] # since prophet makes future predictions, fcast.pers is future data that is removed

variable.names(mudac)
proph.var = proph.var %>% rename("corn" = V1, 
                                 "ethanol" = V2, 
                                 "steel" = V3,
                                 "march.soy" = V4,
                                 "may.soy" = V5,
                                 "july.soy" = V6)

proph.var$ds = y.m.d # adds the date to the data frame

### Then to create a VAR of the non-differenced data :

attach(proph.var)
corn = proph.var$corn
ethanol = proph.var$ethanol 
steel = proph.var$steel
march.soy = proph.var$march.soy
may.soy = proph.var$may.soy
july.soy = proph.var$july.soy
non.diff.data = cbind(corn, 
                      ethanol, 
                      steel,
                      march.soy,
                      may.soy,
                      july.soy) # positions the data so it can be used for create a VAR 

lagmax = 20 # set this to a low-ish number as maximim possible lags
lag.sel = lags.select(non.diff.data, lag.max = lagmax) # finds optimal number of lags by looking at min info criteria in lagmax
lag.sel
num.lags = seq(1,20)
lags = cbind(num.lags,
             lag.sel$HQs)
lag.sel.best = lag.sel$HQ_min[2]

mod.proph.var = lineVar(non.diff.data, lag = lag.sel.best) # this regression uses HQ as the IC to decide lag 


test.preds = predict(mod.proph.var, n.ahead = 183) # this creates predictions 92 days ahead [Nov 30]
preds = as.data.frame(test.preds)
preds.march = preds$march.soy 
preds.may = preds$may.soy
preds.july = preds$july.soy

fits = as.data.frame(mod.proph.var$fitted.values) 
fits.march = fits$march.soy
fits.may = fits$may.soy
fits.july = fits$july.soy # this is a vector of fitted values for soy closing 

fits.march.mod = c(rep(NA, lag.sel.best), fits.march) # adds NA values to account for lag 
fits.may.mod = c(rep(NA, lag.sel.best), fits.may)
fits.july.mod = c(rep(NA, lag.sel.best), fits.july)

predict.line.MAR = c(fits.march.mod, preds.march) # combines fitted values and preditions 
predict.line.MAY = c(fits.may.mod, preds.may)
predict.line.JUL = c(fits.july.mod, preds.july)

par(mar=c(5.1, 4.1, 4.1, 2.1)) # fits plot dimensions

plot.ts(mudac$marchSoybean, xlim = c(0,length(predict.line.MAR)), ylab = "Price", main = "March Predictions") # plots actual values 
lines(predict.line.MAR, col = "red") # this draws predictions and fitted values as a red line over scatter plot 

plot.ts(mudac$maySoybean, xlim = c(0,length(predict.line.MAY)), ylab = "Price", main = "May Predictions") # plots actual values 
lines(predict.line.MAY, col = "red") 

plot.ts(mudac$julySoybean, xlim = c(0,length(predict.line.JUL)), ylab = "Price", main = "July Predictions") # plots actual values 
lines(predict.line.JUL, col = "red")  

minnemudac.preds = cbind(preds.march,
                         preds.may,
                         preds.july) # binds all prediction columns together for a table 

head(minnemudac.preds, 5) # this is a table of the final predictions for the competition 

#############################

# THE FOLLOWING ARE USED TO TEST THE MODEL
  
#############################

adf = adf.test(predict.line.MAR, nlag = lag.sel.best) # augmented dickey-fuller test 
adf$type3[8,3] # p value for model with 8 lags


#############################

# THE FOLLOWING ARE USED TO EXPORT .CSV FILES

############################# 

write.csv(proph.var, file = "prophet_fittedvalues.csv") 
write.csv(predict.line.MAR, file = "predictions_March.csv")
write.csv(predict.line.MAY, file = "predictions_May.csv")
write.csv(predict.line.JUL, file = "predictions_July.csv") 
write.csv(lags, file = "lags.csv")

#############################

# THE FOLLOWING ARE USED FOR DATA VIZ 

############################# 

mudac.avg.soy = (mudac$marchSoybean + 
                   mudac$maySoybean + 
                   mudac$julySoybean)/3 

plot.ts(mudac.avg.soy)

date = mudac$Date
y.m.d = format(as.Date(date, "%m/%d/%Y"), '%Y-%m-%d') # prophet requires ds to be %Y-%m-%d instead of mdy
mudac$ymd = y.m.d
varns = variable.names(mudac)[5]
mudac.p = mudac[,c("ymd",varns)]
mudac.p
mudac.p$ds = y.m.d 
mudac.p$y = mudac.p[,2] 
d = mudac.p[,c("ds","y")] # prophet requires names "ds" for datestamp and "y" for time series variable
pro = prophet(d, weekly.seasonality = TRUE, yearly.seasonality = TRUE, daily.seasonality = TRUE)
future = make_future_dataframe(pro, periods = 30)
forecast = predict(pro, future)
plot(pro, forecast, xlab = "Date", ylab = "March Soybean Price") 
prophet_plot_components(pro, forecast)
