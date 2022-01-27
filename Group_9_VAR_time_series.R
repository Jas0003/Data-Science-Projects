library(kernlab)
library(data.table)
library(ggplot2)
library(caret)
library(mice)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(reshape)
library(mlbench)
library(Boruta)
library(forecast)
library(marima)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(ggcorrplot)
library(urca)
library(Metrics)
library(mvabund)

set.seed(123)
#This dataset has 4 controls, which means that there is data for 4 different drivers for 1 car
#Analysis can be performed for all 4 driver datasets in the file
data = fread("exp3_4drivers_1car_1route.csv")
summary(data)
#Select control1 data and remove null columns
data = data[data$VEHICLE_ID=='control2',]
data = data[, c("TERM FUEL TRIM BANK 1","FUEL_ECONOMY","LONG TERM FUEL TRIM BANK 2","FUEL_TYPE","FUEL_PRESSURE","SHORT TERM FUEL TRIM BANK 2","aqui","DTC_NUMBER","VEHICLE_ID","LONGITUDE","LATITUDE","EQUIV_RATIO","BAROMETRIC_PRESSURE","AMBIENT_AIR_TEMP","ord"):=NULL]

#clean trouble code values
data$TROUBLE_CODES[is.na(data$TROUBLE_CODES)]<-0
data$TROUBLE_CODES <- ifelse(data$TROUBLE_CODES > 1,1,0)

#Impute missing values using MICE
datanew = mice(data = data, where = is.na(data), method = 'pmm', seed = 123)
datatest = complete(datanew)
md.pattern(datatest)
sapply(datatest, function(x) sum(is.na(x)))

datatest = data.table(datatest)
datatemp = datatest
corr<-cor(datatemp[,c("ENGINE_RUNTIME","TIME"):=NULL])
ggcorrplot(corr)
index <- findCorrelation(abs(corr), 0.75,exact=FALSE)

#the name of the columns chosen above
to_be_removed <- colnames(corr)[index]

#now go back to df and use to_be_removed to subset the original df
datats=datatemp[,c(to_be_removed):=NULL]

corr<-cor(datats)
ggcorrplot(corr)

#Data scaling 
data.pre <- preProcess(datats, method="range")
datats <- predict(data.pre, datats)

corr<-cor(datats)
ggcorrplot(corr)

# Converting engld, engtmp, airtmp, intpres and speed to time series
engld <- ts(datats$ENGINE_LOAD, start = c(0,1), frequency = 15)
engtmp <- ts(datats$ENGINE_COOLANT_TEMP, start = c(0,1), frequency = 15)
airtmp <- ts(datats$AIR_INTAKE_TEMP, start = c(0,1), frequency = 15)
intpres <- ts(datats$INTAKE_MANIFOLD_PRESSURE, start = c(0,1), frequency = 15)
speed <- ts(datats$SPEED, start = c(0,1), frequency = 15)

#Time series plot of variables 
autoplot(cbind(engld, engtmp))
autoplot(cbind(airtmp,intpres,speed))

ts_plot(engld,Xtitle = "Time", title = "Time series plot - ENGINE_LOAD")
ts_plot(engtmp,Xtitle = "Time", title = "Time series plot - ENGINE_COOLANT_TEMP")
ts_plot(airtmp, Xtitle = "Time", title = "Time series plot - AIR_INTAKE_TEMP")
ts_plot(intpres, Xtitle = "Time", title = "Time series plot - INTAKE_MANIFOLD_PRESSURE")
ts_plot(speed, Xtitle = "Time", title = "Time series plot - SPEED")


# Check if data is stationary using ADF unit root test
adf.test(engld)
adf.test(engtmp)
adf.test(airtmp)
adf.test(intpres)
adf.test(speed)

#Ideal number of times diff() should be applied using ADF and KPSS
engld %>% ndiffs(test = "kpss") 
engtmp %>% ndiffs(test = "kpss") 
airtmp %>% ndiffs(test = "kpss") 
intpres %>% ndiffs(test = "kpss") 
speed %>% ndiffs(test = "kpss") 

engld %>% ndiffs(test = "adf") 
engtmp %>% ndiffs(test = "adf") 
airtmp %>% ndiffs(test = "adf") 
intpres %>% ndiffs(test = "adf") 
speed %>%  ndiffs(test = "adf") 

#Performing first order differencing technique on the data
engld = diff(engld)
engtmp = diff(engtmp)
airtmp = diff(airtmp)
intpres = diff(intpres)
speed = diff(speed)

# Verifying that first order differencing is enough to transform the data
adf.test(engld)
adf.test(engtmp)
adf.test(airtmp)
adf.test(intpres)
adf.test(speed)

ts_plot(engld,Xtitle = "Time", title = "Time series plot after differencing - ENGINE_LOAD")
ts_plot(engtmp,Xtitle = "Time", title = "Time series plot after differencing - ENGINE_COOLANT_TEMP")
ts_plot(airtmp, Xtitle = "Time", title = "Time series plot after differencing - AIR_INTAKE_TEMP")
ts_plot(intpres, Xtitle = "Time", title = "Time series plot after differencing - INTAKE_MANIFOLD_PRESSURE")
ts_plot(speed, Xtitle = "Time", title = "Time series plot after differencing - SPEED")

v1 <- cbind((engld),(engtmp),(airtmp),(intpres),(speed))
colnames(v1) <- cbind("ENGINE_LOAD","ENGINE_COOLANT_TEMP","AIR_INTAKE_TEMP","INTAKE_MANIFOLD_PRESSURE","SPEED")

#Test train split
rows = nrow(v1)
test = v1[c(rows-20:rows),c(1:5)]
v1 = v1[c(1:(rows-20)),c(1:5)]

#Find optimal lag order using AIC, HQIC, FPE, and the SBIC 
lagselect <- VARselect(v1, lag.max = 15, type = "const")
lagselect$selection

Model1 <- VAR(v1, p = 6, type = "const", season = NULL, exog = NULL) 
summary(Model1)

#Portmanteau Test (asymptotic) for checking serial correlation in errors
Serial <- serial.test(Model1, lags.pt = 15, type = "PT.asymptotic")
Serial


#Check for structural breaks in the model
Stability <- stability(Model1, type = "OLS-CUSUM")
par(mar=c(1,1,1,1))
plot(Stability)

#Check for Heteroscedasticity
Arch <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch

#Granger Causality for all 5 variables
GrangerENGLD<- causality(Model1, cause = "ENGINE_LOAD")
GrangerENGLD
GrangerENGTMP<- causality(Model1, cause = "ENGINE_COOLANT_TEMP")
GrangerENGTMP
GrangerAIT <- causality(Model1, cause = "AIR_INTAKE_TEMP")
GrangerAIT
GrangerIMP <- causality(Model1, cause = "INTAKE_MANIFOLD_PRESSURE")
GrangerIMP
GrangerSPEED <- causality(Model1, cause = "SPEED")
GrangerSPEED

#Forecast error variance decomposition
FEVD <- fevd(Model1, n.ahead = 10)
FEVD
plot(FEVD)


forecast <- predict(Model1, ci = 0.95, n.ahead=10)

fanchart(forecast, names = "ENGINE_LOAD", main = "Fanchart for ENGINE_LOAD", xlab = "Horizon", ylab = "ENGINE_LOAD")
fanchart(forecast, names = "ENGINE_COOLANT_TEMP", main = "Fanchart for ENGINE_COOLANT_TEMP", xlab = "Horizon", ylab = "ENGINE_COOLANT_TEMP")
fanchart(forecast, names = "AIR_INTAKE_TEMP", main = "Fanchart for AIR_INTAKE_TEMP", xlab = "Horizon", ylab = "AIR_INTAKE_TEMP")
fanchart(forecast, names = "INTAKE_MANIFOLD_PRESSURE", main = "Fanchart for INTAKE_MANIFOLD_PRESSURE", xlab = "Horizon", ylab = "INTAKE_MANIFOLD_PRESSURE")
fanchart(forecast, names = "SPEED", main = "Fanchart for SPEED", xlab = "Time", ylab = "SPEED")
forecast



