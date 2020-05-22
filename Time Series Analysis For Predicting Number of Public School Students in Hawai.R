#Created By: Gita Kartika Suriah

#Call Package
library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(ggplot2)
library(forecast)
library(lmtest)

#Check Missing Data
Public_School<-Time_Series_Analysis_Project_Public_School_Database
is.na(Public_School)

#Check Outlier
names(Public_School)<- "Public_School"
ggplot(Public_School,aes(x=1,y=Public_School))+geom_boxplot()

#Create data time series
public<-ts(Public_School,frequency=1,start=1959) 
plot(public)

###STASIONERITY TEST###
adf.test(public)

#Differencing 1
diff1<-diff(public,difference=1)
plot(diff1)
adf.test(diff1) 

#Differencing 2
diff2<-diff(public,difference=2)
plot(diff2)
adf.test(diff2) 

#Time Series Plot, ACF, dan PACF
tsdisplay(diff2)

#Possible ARIMA Models
eacf(diff2)

###DETERMINE BEST MODEL###
#1.Construct Possible Model
model1<-arima(public, order=c(0,2,1)) 
model2<-arima(public, order=c(0,2,2))
model3<-arima(public, order=c(1,2,2))
cbind(model1,model2,model3) 

#Model1:Smallest AIC &BIC value
#Model3: Largest LogLikelihood value
#Model Check
auto.arima(public)

#2.Check whether model is seasonal or not
auto.arima(public)
auto.arima(public, seasonal=FALSE)

###RESIDUAL CHECK####
#1.Independent Test
checkresiduals(model1)

#2.Normality Test
qqnorm(public)
shapiro.test(model1$residuals)
jb.norm.test(public,nrepl=2000)

#3.Check whether the residual is white noise or not
acf(model1$residuals)
pacf(model1$residuals)
eacf(model1$residuals)

#4.Show Residual Model Variance < Data Variance
var(public)
var(model1$residuals)

###PARAMETER ESTIMATION###
arima(public, order=c(0,2,1), method='ML') 
arima(public, order=c(0,2,1), method='CSS') 
# Model 1: Wt= Et - 0.857Et
coeftest(model1)
         
###OVERFIT###
overfit1<-arima(public, order=c(1,2,1))
model1;overfit1 
coeftest(overfit1)
overfit2<-arima(public, order=c(0,2,2))
model1;overfit2
coeftest(overfit2)

###CROSS VALIDATION###
#Next 5 Years
actual1=window(public,start=c(2014))
public1=window(public, end=c(2013))
publicmodel1<-Arima(public1, order=c(0,2,1))
fc1<-forecast(publicmodel1, h=5)
plot(fc1)
cbind(actual1,fc1)
publicmodel1

###FORECASTING###
forecast<-forecast(model1, h=5)
plot(forecast)
forecast
accuracy(forecast)