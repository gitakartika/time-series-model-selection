#Created By: Gita Kartika Suriah

#Call Package
library(TSA)
library(forecast)
library(tseries)
library(normtest)
library(ggplot2)

#Check Missing Data
Private_School<-Time_Series_Analysis_Project_Private_School_Database
is.na(Private_School)

#Check Outlier
ggplot(Private_School,aes(x=1,y=Private_School))+geom_boxplot()
names(Private_School)<- "Private_School"

#Create data time series
private<-ts(Private_School,frequency=1,start=1959) 
plot(private)

###STASIONERITY TEST###
adf.test(private)

#Differencing 1
diff<-diff(private,difference=1)
plot(diff)
adf.test(diff) 

#Time Series Plot, ACF, dan PACF
tsdisplay(diff)

#Possible ARIMA Models
eacf(diff)

###DETERMINE BEST MODEL###
#1.COnstruct possible model
model1<-Arima(private, order=c(0,1,0),include.drift = TRUE) 
model2<-Arima(private, order=c(1,1,0),include.drift = TRUE)
model3<-Arima(private, order=c(0,1,1),include.drift = TRUE)
cbind(model1,model2,model3) 

#Model1:Smallest AIC &BIC value
#Model3: Largest LogLikelihood value 

#Model Check 
auto.arima(private)

#2.Check whether model is seasonal or not
auto.arima(private)
auto.arima(private, seasonal=FALSE)

###RESIDUAL CHECK####
#1.Independent Test
checkresiduals(model1)

#2.Normality Test
qqnorm(private)
shapiro.test(model1$residuals)
jb.norm.test(private,nrepl=2000)

#3.Check whether the residual is white noise or not
acf(model1$residuals)
pacf(model1$residuals)
eacf(model1$residuals)

#4.Show Residual Model Variance < Data Variance
var(private)
var(model1$residuals)

###OVERFIT###
overfit1<-Arima(private, order=c(1,1,0),include.drift = TRUE)
model1;overfit1 
overfit2<-Arima(private, order=c(0,1,1),include.drift = TRUE)
model1;overfit2

###CROSS VALIDATION###
#Next 5 Years
actual1=window(private,start=c(2014))
private1=window(private, end=c(2013))
privatemodel1<-Arima(private1, order=c(0,1,0),include.drift = TRUE)
fc1<-forecast(privatemodel1, h=5)
plot(fc1)
cbind(actual1,fc1)
privatemodel1

###FORECASTING###
forecast<-forecast(model1, h=5)
plot(forecast)
forecast
accuracy(forecast)
