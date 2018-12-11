# Final Project
# STAT 4825 - Time Series
# Emma Frawley
# Colin Mortimer

# 372 observations, by month
# Covers the entire time period from 1986 - 2016

# Pre-analysis code

set.seed(2018)
library(tseries)
library(forecast)
library(fpp)
library(ggplot2)
library(caTools)
library(stringr)
library(readr)
library(imputeTS)
library(pracma)
library(astsa)
library(car)
library(imputeTS)
library(Hmisc)

# Load data and clean up variable names

Data <- read_csv("Data.csv")
names(Data)<-str_replace_all(names(Data), c(" " = "."))

data_ts = ts(Data, start=1)
data_ts = na.approx(data_ts)
colnames(data_ts)
train = window(data_ts, end = 322)
test = window(data_ts, start = 323)
visits_train = Data$Recreation.Visits[1:322]

# Confirm seasonality
ts.plot(visits_train)
acf(visits_train, main="ACF of visits series")
pacf(visits_train)
# adf.test(visits, k=12)

# Calculate period
s = spec.pgram(visits_train)
summary(s$spec)
s$spec # Visual analysis shows that the max occurs at index 31
period = 1/s$freq[27]
period

# Take the twelfth and first difference
visits_dd = diff(detrend(visits_train), 12)
ts.plot(visits_dd)
acf(visits_dd, main="ACF of detrended visits series w/o seasonality")
pacf(visits_dd,  main="PACF of detrended visits series w/o seasonality")
adf.test(visits_dd, k=12)

# Choose model parameters
fit = auto.arima(train[, "Recreation.Visits"], seasonal=TRUE)
fit

# Fit, predict and evaluate model
model = arima(train[, "Recreation.Visits"], order=c(2,1,2), seasonal = list(order = c(0,1,0), period = 12))
model$aic
forecast = forecast(train[, "Recreation.Visits"], model = model, h=50)
autoplot(forecast, ylab = "Visits") + 
  autolayer(ts(test[, "Recreation.Visits"], start = 323, end=372), series="Actual visits")
accuracy(model)
checkresiduals(model)
coeftest(model)

# Choose model parameters with external predictors
covariates = c("MeanTemperature(F)")
fit2 = auto.arima(train[, "Recreation.Visits"], seasonal=TRUE, xreg=train[, covariates])
fit2

# Fit, predict and evaluate model with external predictors
model2 = Arima(train[, "Recreation.Visits"], order=c(1,1,2), seasonal = list(order = c(0,1,0), period = 12), 
              xreg=train[, covariates])
model2$aic
forecast2 = forecast(model2, xreg=train[, covariates], h=50)
autoplot(forecast2, ylab = "Visits", xlim=c(0, 372)) + 
  autolayer(ts(test[, "Recreation.Visits"], start = 323, end=372), series="Actual visits")
accuracy(model2)
checkresiduals(model2)
coeftest(model2)

#Impute to fill in missing values in the time series
rv<-Data$Recreation.Visits
lowtemp=na.interpolation(Data$`LowestTemperature(F)`)
hightemp=na.interpolation(Data$`HighestTemperature(F)`)
avgtemp=na.interpolation(Data$`MeanTemperature(F)`)
rain=na.interpolation(Data$`TotalPrecipitation(In)`)
snow=na.interpolation(Data$`TotalSnowfall(In)`)
airfare=na.interpolation(Data$`3month.Percent.Change.Airfare.Costs`)
food=na.interpolation(Data$`3month.Percent.Change.Food.Away.From.Home.Costs`)
gas=na.interpolation(Data$`3month.Percent.Change.Gasoline.Costs`)
fuel=na.interpolation(Data$`3month.Percent.Change.Jet.Fuel.Costs`)
cpi=na.interpolation(Data$Consumer.Price.Index)
csi=na.interpolation(Data$Consumer.Sentiment.Index)
unemp=na.interpolation(Data$Unemployment.Rate)


#Make dataset for the variables we are interested in using
Data2 <- cbind(rv,avgtemp,lowtemp,hightemp,rain,snow,airfare,food,gas,fuel,cpi,csi,unemp)
Data3<-as.data.frame(Data2)


# Create training and test split for the data
visits_train = Data3[c(1:288),c(1:13)]
n_train=length(visits_train)
visits_test  = Data3[c(289:372),c(1:13)]
n_test=length(visits_test)

#Create month indicator
per=12
sets=nrow(visits_train)/per
month=factor(rep(1:per,sets))

#Create data set combining training data with month indicator
df1<-data.frame(visits_train,month=month)

#Check for collinearity
ynp.mlr=lm(rv~avgtemp+lowtemp+hightemp+rain+snow+airfare+food+gas+fuel+cpi+csi+unemp,data=Data3)
vif(ynp.mlr) 
#vif for avgtemp is >30 so we will remove that variable to prevent colinearity in the model

#Create first model 
ynp2.mlr=lm(rv~hightemp+lowtemp+rain+snow+airfare+food+gas+fuel+cpi+csi+unemp+month-1,df1)
summary(ynp2.mlr)

#AIC and BIC of first model
AIC.model2=AIC(ynp2.mlr)/nrow(visits_train)
BIC.model2=BIC(ynp2.mlr)/nrow(visits_train)
AIC.model2
BIC.model2

#Create second model 
ynp3.mlr=lm(rv~food+cpi+csi+unemp+month-1,df1)
summary(ynp3.mlr)

#AIC and BIC for second model
AIC.model3=AIC(ynp3.mlr)/nrow(visits_train)
BIC.model3=BIC(ynp3.mlr)/nrow(visits_train)
AIC.model3
BIC.model3

#Create third model
ynp4.mlr=lm(rv~food+csi+unemp+month-1,df1)
summary(ynp4.mlr)

#AIC and BIC for third model
AIC.model4=AIC(ynp4.mlr)/nrow(visits_train)
AIC.model4
BIC.model4=BIC(ynp4.mlr)/nrow(visits_train)
BIC.model4

#AIC lower for the first model but BIC lower for the second model. 
#Will use the simpler of the two models (3rd model)

#Plot 3rd model
quartz()
ts.plot(visits_train,main="Visits, Model fit")
lines(ynp4.mlr$fit,col="blue",lty="dashed")

#3rd model fit diagnostics
quartz()
par(mfrow=c(2,2)) 
plot(ynp4.mlr, main="Model Fit Diagnostics",which = 1:4)

#Create data frame combining test data and month indicator
newdata<-data.frame(visits_test,month=factor(rep(1:per,7)))

#Predictions for test data using third model
pfore.model3=predict(ynp4.mlr,newdata,se.fit=TRUE)
pfore.model3$fit

#Plot of predictions
obs.fit.ts<-cbind(pfore.model3$fit,visits_test$rv)
quartz()
plot.ts(obs.fit.ts, plot.type = "single",col=1:2, main= "Prediction vs. Observed")


