rm(list=ls())
library(readxl)
library(xlsx)
library(fUnitRoots)
library(tseries)
library(forecast)
library(sarima)
library(lmtest)
library(rsq)
library(polynom)
library(fracdiff)
library(hypergeo)
library(sandwich)
library(longmemo)
library(abind)
library(fractaldim)
library(FractalParameterEstimation)
library(liftLRD)
library(pracma)
library(arfima)
library(afmtools)
library(rugarch)
library(stats)
library(LongMemoryTS)
library(NHMSAR)
library(MSGARCH)
library(readxl)
library(xlsx)
library(fUnitRoots)
library(tseries)
library(forecast)
library(sarima)
library(lmtest)
library(rsq)
library(strucchange)
# set working directory in which you want to open and save your extractions

setwd("C:/Users/SMSAR Model using R/results")

#######################################
# Reading (opening) the data file and choosing specific data from it

File<- read_excel("C:/Users/SMSAR Model using R/UKdatabase.xlsx")
data <- read_excel("C:/Users/SMSAR Model/SMSAR Model using R/UKdatabase.xlsx",range = cell_cols("B") )
Time <- read_excel("C:/Users/SMSAR Model using R/UKdatabase.xlsx",range = cell_cols("A"))

#######################################
#Analyzing the data file

sf=summary(File)# provides a minimum value, maximum value, median and the mean for all the embedded data
sf
class(data)# class of the data embedded in the file should be time series
data.ts= ts(data)# creating time series for the variable in the data file- this is useful for strudying the data in the file not taking into consideration the value of the year instead of this it takes them as step points

class(data.ts)# provides the classes in each column detected in the data file


#######################################
# Define 

data.month=ts(data,start=c(1883,1),end = c(2020,12),freq=12)#Defining a time series data inserting their time value and step #This uses all the data. You can select a smaller number by specifying a nearlier end date using the parameter end
class(data.month)
sd=summary(data.month)
sd
plot(data.month)# plotting the time series 
bp=boxplot(data.month ~ cycle(data.month))#You can see  the seasonal effects in the boxplot
bpstats=bp$stats#	a matrix, each column contains the extreme of the lower whisker, the lower hinge, the median, the upper hinge and the extreme of the upper 
#whisker for one group/plot. If all the inputs have the same class attribute, so will this component.
bpconf=bp$conf#a matrix where each column contains the lower and upper extremes of the notch.
write.xlsx(
  bpstats,
  file="Boxwhisker.xlsx",
  sheetName = "States",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
write.xlsx(
  bpconf,
  file="Boxwhisker.xlsx",
  sheetName = "Conf",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)

#######################################
# Visualize the seasonal effect detected in specefic months starting from 1883 

data.janv=window(data.month, start=c(1883,1),frequency=TRUE)
data.janv
var(data.janv)
data.fev=window(data.month, start=c(1883,2),frequency=TRUE)
data.fev
data.mars=window(data.month, start=c(1883,3),frequency=TRUE)
data.mars
data.avril=window(data.month, start=c(1883,4),frequency=TRUE)
data.avril
data.mai=window(data.month, start=c(1883,5),frequency=TRUE)
data.mai
data.june=window(data.month, start=c(1883,6),frequency=TRUE)
data.june
data.juillet=window(data.month, start=c(1883,7),frequency=TRUE)
data.juillet
data.aout=window(data.month, start=c(1883,8),frequency=TRUE)
data.aout
data.septembre=window(data.month, start=c(1883,9),frequency=TRUE)
data.septembre
data.octobre=window(data.month, start=c(1883,10),frequency=TRUE)
data.octobre
data.novembre=window(data.month, start=c(1883,11),frequency=TRUE)
data.novembre
data.decembre=window(data.month, start=c(1883,12),frequency=TRUE)
data.decembre

plot(data.janv)# increase in seasonal trend starting from 1950 specially each 6 years a big peak
m1=mean(data.janv)
v1=var(data.janv)
#high mean and variance
plot(data.fev)#increase in seasonal trend in general
m2=mean(data.fev)
v2=var(data.fev)
#high mean and variance
plot(data.mars)#no increase in seasonal trend
m3=mean(data.mars)
v3=var(data.mars)
# low mean and variance
plot(data.avril)#no increase in seasonal trend
m4=mean(data.avril)
v4=var(data.avril)
# low mean and variance
plot(data.mai)#no increase in seasonal trend
m5=mean(data.mai)
v5=var(data.mai)
# low mean and variance
plot(data.june)# no increase in seasonal trend
m6=mean(data.june)
v6=var(data.june)
# low mean and variance
plot(data.juillet)#no increase in seasonal pattern at all except for 2 peaks in 2009 and 2011
m7=mean(data.juillet)
v7=var(data.juillet)
# low mean and variance
plot(data.aout)#no increase in seasonal pattern  
m8=mean(data.aout)
v8=var(data.aout)
# low mean and variance
plot(data.septembre)# no increase in seasonal trend
m9=mean(data.septembre)
v9=var(data.septembre)
# low mean and variance
plot(data.octobre)# decrease in seasonal trend
m10=mean(data.octobre)
v10=var(data.octobre)
# low mean and variance
plot(data.novembre)# no increase in seasonal trend despite that there are some peaks
m11=mean(data.novembre)
v11=var(data.novembre)
# low mean and variance
plot(data.decembre)#no increase in seasonal trend but peaks are detected
m12=mean(data.decembre)
v12=var(data.decembre)
monthsvalues=data.frame(data.janv,data.fev,data.mars,data.avril,data.mai,data.june,data.juillet,data.aout,data.septembre,data.octobre,data.novembre,data.decembre)
monthsmeanvalues=data.frame(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
monthsvariancevalues=data.frame(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
#Saving
write.xlsx(
  monthsvalues,
  file="Dataset description.xlsx",
  sheetName = "Months values",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)
write.xlsx(
  monthsmeanvalues,
  file="Dataset description.xlsx",
  sheetName = "Months mean values",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
write.xlsx(
  monthsvariancevalues,
  file="Dataset description.xlsx",
  sheetName = "Months variance values",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
write.xlsx(
  ts(sd),
  file="Dataset description.xlsx",
  sheetName = "dataset summary",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
#####visualiez the seasonal effect conclusion:
##An increase in the seasonal trend for janvier et fevrier, high mean values for both and for decembre and mars. high variance for janvier et fevrier then for december and mars is the lowest between them
## in General seasonal increase detected for january and february, In descending order the mean and the variance values are as follows: January, February, December and Mars
#######################################
#Aggregation
#used to detect the total number of River flow in each year and give a view without the seasonal effect

ag=aggregate(data.month)# total number of m3/sec in each year=summation of m3/sec in each year
plot(aggregate(data.month))# used To get a clearer view of the trend, the seasonal effect can be removed by aggregating the data
mean(ag)
var(ag)

do=density(ag,n = 60);plot(do)
do$x
do$y# density
denistyframeoriginal=data.frame(do$x,do$y)
colnames(denistyframeoriginal) <- c("Xaxis","density")

m=mean(ag)
sd=sd(ag)

x <- rnorm(1630, mean=m, sd=sd)
hist(x, probability=TRUE)
xx <- seq(min(x), max(x), length=2000)
xxx=dnorm(xx, mean=m, sd=sd)
lines(xx, dnorm(xx, mean=m, sd=sd))


#######################################
#######################################

#Seasonal Autoregressive integrated moving average model (SARIMA)

#this is performed using a time domain series analysis by fitting a mathematical model to time series of data, in the SARIMA
#a stationary phase must be achieved ( checked by Unit root test) 

#Differencing:

#Determine if differencing is required or not by Determining the stationarity of the time series 

#1-Unit Root Test Interface
#0: unit root test for stationarity
#provides acf,pacf and regression test tau residuals
#urkpssTest(x, type = c("mu", "tau"), lags = c("short", "long", "nil"), use.lag = NULL, doplot = TRUE)
urkpssTest(data.month, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
# due to ACF and PACF the dataset required and AR model with seasonality effect and an MA model is also required with seasonal component
#2-kpss.test
#Kpss test indicates the time series stationarity status and indicates with the value of pvalue if differencing is requried
#kpss.test(x, null = c("Level", "Trend"), lshort = TRUE)
#if the Pvalue<0.05 and statistic level  > critical  then the null hypothesis is rejected; the series is non-stationary. 
#if the Pvalue>0.05 and statistic level  < critical  then the null hypothesis is accepted; the series is stationary. 

#critical value of level
#Sample 	1%	  intercept	    linear trend	  
#alpha = 0.1	  0.347	  	       0.119	
#alpha = 0.05   0.463		         0.146	
#alpha = 0.01	  0.739	         	 0.216	

#Kpss Trend test

kpsstesttrend=kpss.test(data.month, null = c("Trend"),lshort = TRUE)
kpsstesttrend$statistic# value compared with the critical
kpsstesttrend$parameter
kpsstesttrend$p.value # value compared with 0.05

KpssTesttrendpvalue=c(kpsstesttrend$p.value)
KpssTesttrendpvalue
kpssTesttrendstatistic=c(kpsstesttrend$statistic)
kpssTesttrendstatistic
pCriteria=c(0.05)
pCriteria
kpsstest.resulttrend=ifelse(KpssTesttrendpvalue<pCriteria && kpssTesttrendstatistic>0.739,"Time series is non stationary","Time series is stationary")
kpsstest.resulttrend

#Kpss Level test

kpsstestLevel=kpss.test(data.month, null = c("Level"),lshort = TRUE)
kpsstestLevel$statistic# value compared with the critical
kpsstestLevel$parameter
kpsstestLevel$p.value # value compared with 0.05

KpssTestLevelpvalue=c(kpsstestLevel$p.value)
KpssTestLevelpvalue
kpssTestLevelstatistic=c(kpsstestLevel$statistic)
kpssTestLevelstatistic
pCriteria=c(0.05)
pCriteria
kpsstest.resultLevel=ifelse(KpssTestLevelpvalue<pCriteria && kpssTestLevelstatistic>0.739,"Seasonal Time series is non stationary","Seasonal Time series is stationary")
kpsstest.resultLevel

#3-ADf test

#Critical values for Dickey-Fuller t-distribution.
#        Without trend  	With trend
#Sample size	1%	5%	    1%	  5%
#T = 25	  3.75	3.00	  4.38	  3.60
#T = 50	  3.58	2.93	  4.15	  3.50
#T = 100	3.51	2.89	  4.04	  3.45
#T = 250	3.46	2.88	  3.99	  3.43
#T = 500	3.44	2.87	  3.98	  3.42
#T =+500  3.43	2.86	  3.96	  3.41
#if the Pvalue >0.05 and adf statistic > critical then it is non stationatry time series
#if the pvalue <0.05 and adf statistic < critical then it is  stationatry time series

#adf.test(x, alternative = c("stationary", "explosive"),
#         k = trunc((length(x)-1)^(1/3)))

# Consequtive adf test time series
adfcons=adf.test(data.month,alternative = c("stationary"), k = trunc((length(data.month)-1)^(1/3)))#k=1 can be used too 
adfcons
adfcons$statistic
adfcons$p.value
# Null hypothenous as pvalue <0.05 and adf statistic < critical  then it is  stationatry time series

adfpvalue=c(adfcons$p.value)
adfpvalue
adfstatistic=c(adfcons$statistic)
adfstatistic
Criteria=c(0.05)
Criteria
adfresult=ifelse(adfpvalue>Criteria ,"Time series is non stationary","Time series is stationary")
adfresult

#Seasonal adf test time series
adftestseas=adfTest(data.month,lags=12,type=c("ct"),title = NULL, description = NULL)# seasonal effect
adftestseas
#the Pvalue<0.05 and adf statistic > critical then it is stationatry seasonal time series

adfseaspvalue=c(adftestseas@test$p.value)
adfseaspvalue
adfseasstatistic=c(adftestseas@test$statistic)
adfseasstatistic
Criteria=c(0.05)
Criteria
adfseasresult=ifelse(adfseaspvalue>Criteria ,"Seasonal Time series is non stationary","Seasonal Time series is stationary")
adfseasresult

# Statistical test shows that the time series is stationary


#Evaluating the integration value 
#determine the number of differences required to make the time series stationary removing seasonality
#estimates the number of seasonal differences necessary.
differencing=nsdiffs(data.month,alpha=0.05, test = c("seas"),max.D = 2)
#seasonality integration is not an integral value which means fractional integration is required

##### Differencing conclusion:
##kpss test result is : Pvalue>0.05 and statistic level  > critical  then the null hypothesis is rejected; part of the series is non-stationary. 
##adf test provides a stationary seasonal time series and stationary consequtive time series
## evaluation of the integration value is indicated to be 0 taking into account the seasonality effect, therefore no integration or fractional integration will be required

# All the previous results indicate that the time series is stationary and therefore, fractional integration for long-term analysis is required or switching regimes models
statisticsconclusion=data.frame("stationary tests results indicate that the time series is stationary and integration is useless and therefore, fractional integration for long-term analysis is required or switching regimes models")
#saving
stationaritytests=data.frame(kpsstest.resulttrend,kpsstest.resultLevel,adfresult,adfseasresult,differencing,statisticsconclusion)
write.xlsx(
  stationaritytests,
  file="Dataset description.xlsx",
  sheetName = "stationary summary",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
#######################################
#######################################

#Mean annual rate

data.AR=aggregate(data.month)/12# average in each year=mean annual rate
data.ARcheck=aggregate(data.month,FUN = mean)# =same of previously used to check the frequency
data.AR
data.ARcheck
plot(data.ARcheck)

##### Mean annual rate conclusion:
## constant trend for the mean annual rate despite that there is peaks  

#######################################

# Month attribution:
#This describe the increase or the decrease of each month in the terms of percentage over the mean of all months in the covered period

janv.ratio=(mean(data.janv)/mean(data.month))*100
fev.ratio=(mean(data.fev)/mean(data.month))*100
mars.ratio=(mean(data.mars)/mean(data.month))*100
avril.ratio=(mean(data.avril)/mean(data.month))*100
mai.ratio=(mean(data.mai)/mean(data.month))*100
june.ratio=(mean(data.june)/mean(data.month))*100
juillet.ratio=(mean(data.juillet)/mean(data.month))*100
aout.ratio=(mean(data.aout)/mean(data.month))*100
septembre.ratio=(mean(data.septembre)/mean(data.month))*100
octobre.ratio=(mean(data.octobre)/mean(data.month))*100
novembre.ratio=(mean(data.novembre)/mean(data.month))*100
decembre.ratio=(mean(data.decembre)/mean(data.month))*100

months.ratio=rbind(janv.ratio,fev.ratio,mars.ratio,avril.ratio,mai.ratio,june.ratio,juillet.ratio,aout.ratio,septembre.ratio,octobre.ratio,novembre.ratio,decembre.ratio)
months.ratio
plot(months.ratio)
write.xlsx(
  months.ratio,
  file="Dataset description.xlsx",
  sheetName = "Months mean ratio",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
###############################################
janv.ratiov=(var(data.janv)/var(data.month))*100
fev.ratiov=(var(data.fev)/var(data.month))*100
mars.ratiov=(var(data.mars)/var(data.month))*100
avril.ratiov=(var(data.avril)/var(data.month))*100
mai.ratiov=(var(data.mai)/var(data.month))*100
june.ratiov=(var(data.june)/var(data.month))*100
juillet.ratiov=(var(data.juillet)/var(data.month))*100
aout.ratiov=(var(data.aout)/var(data.month))*100
septembre.ratiov=(var(data.septembre)/var(data.month))*100
octobre.ratiov=(var(data.octobre)/var(data.month))*100
novembre.ratiov=(var(data.novembre)/var(data.month))*100
decembre.ratiov=(var(data.decembre)/var(data.month))*100

months.ratiov=rbind(janv.ratiov,fev.ratiov,mars.ratiov,avril.ratiov,mai.ratiov,june.ratiov,juillet.ratiov,aout.ratiov,septembre.ratiov,octobre.ratiov,novembre.ratiov,decembre.ratiov)
months.ratiov
plot(months.ratiov)
write.xlsx(
  months.ratiov,
  file="Dataset description.xlsx",
  sheetName = "Months variance ratio",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
##Conclusion:
#The percentage of each month based on the avereage of the dataset provides:
# and increase for january, february, mars and december
# the percentage increase of each month's variance indicate tehat janv,fev, decembre has an increase effect


#Decomposition
#Decomposition model:
#additive:Xt=mt+st+zt 
#multiplicative:Xt=mt.st+zt : used when the seasonal effect increases with the increase of the trend
#Xt:observed data, mt:trend, st:seasonal, zt:random variable with mean equals to zero

Decomposition=decompose(data.month,type = c("multiplicative"),filter = NULL);Decomposition
plot(Decomposition)
Decomposition$x #The original series
Decomposition$seasonal #The seasonal component which is the repeated seasonal figure. using a moving average method
Decomposition$trend# The trend componenet. using a moving average method
Decomposition$random# The remainder part which is an estimate of the random process is obtained from the original time series using estimates of the trend and seasonal effects. it is a residual error series and it is not precisely a realisation of the random process zt  but rather an estimate of that realisation.
Decomposition$figure # the seasonal figure which means seasonal pattern = months.ratio

plot(Decomposition$x)
plot(Decomposition$seasonal)
plot(Decomposition$figure)
plot(Decomposition$trend)
plot(Decomposition$random)
length(Decomposition$random)
#Checks on random part
mean(Decomposition$random[7:1649])#check the value: distribution is around it depending on the variance
var(Decomposition$random[7:1649])#check the value, smaill variance must be detected 

#Effect of seasonality on trend
plot(Decomposition$trend,ylim=c(1,300))
lines(Decomposition$trend*Decomposition$seasonal,col="blue")
ts.plot(cbind(Decomposition$trend,Decomposition$trend*Decomposition$seasonal),lty=1:2)# dotted is the seasonal effect on trend

#Evaluation of the seasonal pattern accuracy
#decides if the type of the decomposition is accepted by calculating the errors 
error.Decomposition=(Decomposition$figure)*100-months.ratio
error.Decomposition
plot(error.Decomposition)
edmean=mean(error.Decomposition);edmean# nearly equal to zero
# decomposition of the dataset demonstrates that the data includes seasonality effect
#Autocorrelation usage in detecting the:

#type of time analysis: 
# the acf and pacf are used to define the time series behaviour and what type of time series analysis to be used:
# ion this case since the acf presents slow decrease towards zero: the long-term memory process or switching regimes can be used
#trend effect: 
#high value at lag 1 & decrease with the increase of lags
#seasonality:autocorrelation will be larger with peaks at seasonal data
# Check if we need AR model or we can use other features;
acf(data.month)$acf[2]#check type of time series analysis if it is short or long
acfdata=acf(data.month, type= c("correlation"), lag.max = 30)# used to detect seasonality for 2 years
pacf(data.month)
# tha acf reaches the zero state 3 times and the PACF presents no zero values for the first p states and zero values elsewhere therefore: AR model is required
acf(Decomposition$random[7:1649])# damaged cosine shape is  definitly a characteristic of AR model 

timeseriestype=data.frame("the acf of the time series presents longterm behaviour, the acf reaches the zero state 3 times and the PACF presents no zero values for the first p states and zero values elsewhere therefore: AR model is required, in addition, the acf of the decomposition of the random part of the dataset presents that: damaged cosine shape is  definitly a characteristic of AR model ")
write.xlsx(
  timeseriestype,
  file="Dataset description.xlsx",
  sheetName = "Time series type and AR need",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)

# the objective to test the no. of autoregressive parameters and their effect on the timeseries without including a markov-switching process

fit2 <- auto.arima(data.month,max.p = 4,max.q = 0,max.P = 4,max.Q = 0,max.d = 2,max.D = 2,max.order = 6,seasonal = TRUE,ic = c("aicc", "aic", "bic"),test ="kpss",trace = TRUE  ,allowdrift = TRUE,nmodels = 1875000)
fit1 <- auto.arima(data.month,max.p = 2,max.q = 0,max.P = 2,max.Q = 0,max.d = 2,max.D = 2,max.order = 6,seasonal = TRUE,ic = c("aicc", "aic", "bic"),test ="kpss",trace = TRUE  ,allowdrift = TRUE,nmodels = 1875000)

# The results from the best fitting models present the following:
#1- AR model with 2 parameters requires MA model in addition, Ar model with 4 states is able to present the data as a short term time series 
#2- An AR model is required ranging to 5 parameter and for its seasonality and AR with 2 parameter, MA model is not required
#3- Therefore, dealing with a long-term switching regimes models a minimum of 2 AR is applicable 
sar=summary(fit1)
# ar2= -ve: means that after a high value of y(t) the y(t+1) will be small values
#ar1=+ve: means that the current value of the time series is positively correlated with its past values. This means that if the past values of the time series were higher or lower than usual, then the current value is likely to be higher or lower than usual as well.
#seasonality ar +ve: means that the seasonality is based on the previously state and this is refused. Therefore, a transition between states is demanded using MSAR model
fit1$residuals
fit1$xreg#drift value
fit1$sigma2#the MLE of the innovations variance
fit1$aicc#corrected Akaike's Information Criterion
fit1$bic#Bayesian Information Criterion, choose lowest
fit1$loglik#value used in the calculation of aic,bic,aicc
fit1$aic#Akaike's Information Criterion , choose lowest value
fit1$x# real data=C02.month
fit1$fitted# data fitted by the model for the current state in which prediction will be made on it

#######################################

#Accuracy of model:

#. ME: Mean Error:the difference between the measured value and true/correct value
#. MAE: Mean Absolute Error :measures the average magnitude of the errors in a set of forecasts( it measures the accuracy)
#. RMSE: Root Mean Squared Error:The RMSE will always be larger or equal to the MAE;the greater difference between them, the greater the variance in the individual error
#. MPE: Mean Percentage Error: percentage of the (ME)
#. MAPE: Mean Absolute Percentage Error :1-Mape= model's accuracy in predicting
#. MASE: Mean Absolute Scaled Error : compare this with the other model without drift
#. ACF1: Autocorrelation of errors at lag 1.:t is a measure of how much is the current value influenced by the previous values in a time series.
#check for small value of MAPE
#The mean absolute percentage error (MAPE) is the mean or average of the absolute percentage errors of forecasts. 
#its error is defined as actual or observed value minus the forecasted value, it provides errors in terms of percentage

accfit1=accuracy(fit1)
accfit1
summary(fit1)


# Rsquare value

df.fit1observed = as.data.frame(fit1$x)
df.fit1observed

df.fit1fitted.R2 = as.data.frame(fit1$fitted)
df.fit1fitted.R2 

df.fit1fitted.R2ts=ts(df.fit1fitted.R2)
df.fit1observedts=ts(df.fit1observed)
R2SARIMAwithdrift=rsq(glm(df.fit1observedts~df.fit1fitted.R2ts),adj = FALSE)
R2SARIMAwithdrift# Rsquare value of the short memory process used to detect AR parameters

#######################################

#CHECK:
#CHECK:Computes confidence intervals for  parameters in a fitted model.
#this describes how tightly you have determined these values. 
#If a confidence interval is very wide(max:1.96), your data don't define that parameter very well
confint(fit1,level = 0.95) #must be lower than 1.96
# AR 1 or 2 can be used and for seasonality and AR 2 is required

#check the residuals of the fitted model:
#Pvalue< 0.05 and Acf should be embedded in the boundary  
# it is common to find that the LB test fails on residuals even though the forecasts might be good using Pvalue<0.05 as a threshold
#if it is > 0.05 then it suggests that there is a little more information in the data than is captured in the model.
#lag=For seasonal time series, use h=  min(  2 m ,  n/5).;m= is the period of seasonality & where n is the length of the series.
tsdiag(fit1) # Check ACF
acf(fit1$residuals)  #check ACF
#AR model is not able to maintain the assumption of IID
write.xlsx(
  summaryar,
  file="Autoregressive model.xlsx",
  sheetName = "summary of the fitted model",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
write.xlsx(
  R2SARIMAwithdrift,
  file="Autoregressive model.xlsx",
  sheetName = "R2",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)

#Ljung Box test

#is a statistical test that checks if autocorrelation exists in a time series and check the prediction models
#Ideally, we would like to fail to reject the null hypothesis. That is, we would like to see the p-value of the test be greater than 0.05 because 
#this means the residuals for our time series model are independent, which is often an assumption we make when creating a model.
#if Pvalue is much larger than 0.05. Thus, we fail to reject the null hypothesis of the test and conclude that the data values are independent.
LJB=Box.test.lagvalue=min(24,length(data.month)/5)# the value of the lag component used in the BOX.TEST
LJB
BT.C=Box.test(fit1$residuals,lag=24,type ="Ljung-Box",fitdf = 0) # check Pvalue 
BT.C
BT.C$p.value# if it is > 0.05 then it is accepted check script 401
BT.C$statistic

BT.Cpvalue=c(BT.C$p.value)
BT.Cpvalue
BT.Cstatistic=c(BT.C$statistic)
BT.Cstatistic
pCriteria=c(0.05)
pCriteria
BT.Cresult=ifelse(BT.Cpvalue>pCriteria ,"ljung Box test result is accepted for SARIMA model with drift",
                  "ljung Box test is refused for SARIMA model with drift, which means that errors are not independent")
BT.Cresult
write.xlsx(
  BT.Cresult,
  file="Autoregressive model.xlsx",
  sheetName = "Ljung-box test",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
# The results are demonstrated also by the ACF. In a sense, non dependant errors are detected by crossing the 95% C.I of the ACF. This means that the 2 AR model should be improved. However, based on the ACF result; the model presents good results
checkresiduals(fit1,lag=24)# should indicate a normal distribution function and their ACF is inside the borders

#Z test of coefficients
#value of Pr(>|z|)<0.05    
#the p-value given for the Z-statistic would have to be interpreted as how likely it is that a result as extreme or more extreme than that
#observed would have occured under the null hypothesis. 
# for resutls>0.05: provide evidence that this variable evidence is still is needed
ztest=coeftest(fit1) 
write.xlsx(
  ts(ztest),
  file="Autoregressive model.xlsx",
  sheetName = "ztest",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)
#conclusion
# the model parameters are still required, therefore, ar model os 2 paramters and seasonality failed to present the ar of negative value which means taht the transition form a high value to a lower value is not presented in this model and therefore , a MSAR model is proposed
#######################################
#######################################
# Structural break
# A test is performed to decide wether a switching AR model is required or not
xaxis=array(data=seq(1:1656))
structuralbreakrest=sctest(data.month ~ xaxis, type = "Chow", point = 12)
structuralbreakrest$statistic
structuralbreakrest$p.value
structuralbreakrest$method
structuralbreakrest$data.name
#Since the p-value is less than .05, we can reject the null hypothesis of the test. This means we have sufficient evidence to say that a structural break point is present in the data.
#In other words, two regression lines can fit the pattern in the data more effectively than a single regression line.
################################################################################
# 1- Initialisation function for MSAR model fitting

#Initialization before fitting (non) homogeneous Markov switching autoregressive 
#models by EM algorithm. Non homogeneity may be introduce at the intercept level
#or in the probability transitions.

#M = number of regimes
#order= AR order
#"HH" (default) for homogeneous MS AR model 
#"HN" for non homogeneous emissions  
#"NH" for non homogeneous transitions
#"NN" for non homogeneous emissions and non homogeneous transitions
datamsar= array((data.month),c(length(data.month),1,1))
plot(datamsar)
theta.init = init.theta.MSAR(datamsar,M=2,order=2,label="HH")
theta.init$A# AR coefficients
theta.init$A0# Intercepts
theta.init$sigma#variances of innovations
theta.init$prior#prior probabilities
theta.init$transmat#transition matrix


write.xlsx(
  theta.init$A,
  file="Initialisation function for MSAR model.xlsx",
  sheetName = "AR coefficients",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)

write.xlsx(
  theta.init$A0,
  file="Initialisation function for MSAR model.xlsx",
  sheetName = "Intercepts",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  theta.init$sigma,
  file="Initialisation function for MSAR model.xlsx",
  sheetName = "variances of innovations",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  theta.init$prior,
  file="Initialisation function for MSAR model.xlsx",
  sheetName = "prior of probabilities",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

write.xlsx(
  theta.init$transmat,
  file="Initialisation function for MSAR model.xlsx",
  sheetName = "transition matrix",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
####################################################################
#2- Fit (non) homogeneous Markov switching autoregressive models

#Fit (non) homogeneous Markov switching autoregressive models by EM
#algorithm. Non homogeneity may be introduced at the intercept level
#or in the probability transition

#ARfix = FALSE ; AR parameters will be estimated not fixed
# method of optimization by default is "ucminf" 
mod.lynx.hh = fit.MSAR(datamsar,theta.init,verbose=TRUE,MaxIter=200,ARfix = FALSE)
mod.lynx.hh$theta# theta initiated by fitting an MSAR model with not fixed AR parameters
mod.lynx.hh[["theta"]][["A"]]# Initialisation ARcoefficients
mod.lynx.hh[["theta"]][["A0"]]# Intercepts
mod.lynx.hh[["theta"]][["sigma"]]#variances of innovations
mod.lynx.hh[["theta"]][["prior"]]#prior of probabilities
mod.lynx.hh[["theta"]][["transmat"]]#transition matrix
mod.lynx.hh$ll_history#log-likelihood for each iterations of the EM algorithm
mod.lynx.hh$Iter#number of iterations run before EM converged
mod.lynx.hh$Npar#number of parameters in the model
mod.lynx.hh$BIC#Bayes Information Criterion
mod.lynx.hh$smoothedprob#smoothing probabilities P(Xt|y0, · · · , yT )
write.xlsx(
  mod.lynx.hh[["theta"]][["A"]],
  file="MSAR model.xlsx",
  sheetName = "Initialisation AR coefficients",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  mod.lynx.hh[["theta"]][["A0"]],
  file="MSAR model.xlsx",
  sheetName = "Intercepts",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  mod.lynx.hh[["theta"]][["sigma"]],
  file="MSAR model.xlsx",
  sheetName = "variance of inovations",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  mod.lynx.hh[["theta"]][["prior"]],
  file="MSAR model.xlsx",
  sheetName = "Prior of probabilities",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

write.xlsx(
  mod.lynx.hh[["theta"]][["transmat"]],
  file="MSAR model.xlsx",
  sheetName = "Transition matrix",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

write.xlsx(
  mod.lynx.hh$smoothedprob,
  file="MSAR model.xlsx",
  sheetName = "Smoothing probability",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
#Plot MSAR time series with regimes materialized by gray boxes.
# red is time series , gray is detected accepted regimes for the model
# regimes extracted are the phase to be followed for each time step performed
regimeplot=regimes.plot.MSAR(mod.lynx.hh,datamsar,ylab="River Flow Regimes captures")
regimeplot.df=as.data.frame(regimeplot[3:1665])
regimeplot.df
regimesextracted=regimeplot.df[,1]

length(regimesextracted)
plot(regimeplot, type = "l", col = "red")
lines(datamsar, type = "l", col = "green")

# Conditional Probability of  MSAR model
CP1 = Cond.prob.MSAR(datamsar, mod.lynx.hh$theta)
CP1$yrange#values at which the conditional probabilities are computed
CP1$prob#conditional probabilities for each time t and each values of yrange
CP1$Yhat#mode of the conditinal distribution for each time t
plot(CP1$prob[1,,],typ="l",main="conditional probabilities",xlab="Time",ylab="Captured")
write.xlsx(
  CP1$yrange,
  file="MSAR model.xlsx",
  sheetName = "values at which CP are computed",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE)

write.xlsx(
  CP1$prob,
  file="MSAR model.xlsx",
  sheetName = "CP & values at which are are computedat each step  ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  CP1$Yhat,
  file="MSAR model.xlsx",
  sheetName = "mode of the CP distribution  ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

ex = 1:1656;ex
lex = length(ex);lex
tps = (1883:2020)[ex]
tps

par(mfrow=c(2,1))
# Plot the Conditional probability in yearly
plot(tps,datamsar,typ="l",main="Homogeneous MSAR model",xlab="Time",ylab="Captured")
lines(tps,CP1$Yhat,col="red")

alpha = .05
IC.emp = matrix(0,2,lex)
for (k in 1:lex) {
  tmp = cumsum(CP1$prob[,k,])/sum(CP1$prob[,k,])
  IC.emp[1,k] = CP1$yrange[max(c(which(tmp<alpha/2),1))]
  IC.emp[2,k] = CP1$yrange[min(max(which(tmp<(1-alpha/2))),length(CP1$yrange))]
}
lines(tps,IC.emp[1,],lty=2,col="blue")
lines(tps,IC.emp[2,],lty=2,col="blue")
IC1y=as.data.frame(IC.emp[1,])
IC2y=as.data.frame(IC.emp[2,])


# Plot the Conditional probability in monthly
plot(ex,datamsar,typ="l",main="Homogeneous MSAR model",xlab="Time",ylab="Captured")
lines(ex,CP1$Yhat,col="red")
alpha = .05
IC.empm = matrix(0,2,lex)
for (k in 1:lex) {
  tmp = cumsum(CP1$prob[,k,])/sum(CP1$prob[,k,])
  IC.empm[1,k] = CP1$yrange[max(c(which(tmp<alpha/2),1))]
  IC.empm[2,k] = CP1$yrange[min(max(which(tmp<(1-alpha/2))),length(CP1$yrange))]
}
lines(ex,IC.empm[1,],lty=2,col="blue")
lines(ex,IC.empm[2,],lty=2,col="blue")

IC1m=as.data.frame(IC.empm[1,])
IC2m=as.data.frame(IC.empm[2,])

CPyearlyandmonthly=data.frame(CP1$Yhat,IC1y,IC2y,tps,datamsar,IC1m,IC2m)
CPyearlyandmonthly

write.xlsx(
  CPyearlyandmonthly,
  file="MSAR model.xlsx",
  sheetName = " CP plot ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
dfobservedcp = as.data.frame(datamsar)
dfobservedcp
dffitted.R2cp=as.data.frame(CP1$Yhat)
dffitted.R2cp

dffitted.R2cpts=ts(dffitted.R2cp)
dfobservedcpts=ts(dfobservedcp)
R2cp=rsq(glm(dfobservedcpts~dffitted.R2cpts),adj = FALSE)
R2cp
write.xlsx(
  R2cp,
  file="MSAR model R2.xlsx",
  sheetName = " R square(mode of distrubution and data) ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
####################################
U=data.month
T = length(U);T
N.samples = dim(U)[2];N.samples
N.samples=1
Y = array(U,c(T,N.samples,1));Y# Y=C02.month
plot(Y)
theta.init1=init.theta.MSAR(Y,M=2,order=2,label="HH")
theta.init1$A# AR coefficients
theta.init1$A0# Intercepts
theta.init1$sigma#variances of innovations
theta.init1$prior#prior probabilities
theta.init1$transmat#transition matrix


write.xlsx(
  theta.init1$A,
  file="Simulated MSAR model.xlsx",
  sheetName = "AR coefficients",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)

write.xlsx(
  theta.init1$A0,
  file="Simulated MSAR model.xlsx",
  sheetName = "Intercepts",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  theta.init1$sigma,
  file="Simulated MSAR model.xlsx",
  sheetName = "variances of innovations",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  theta.init1$prior,
  file="Simulated MSAR model.xlsx",
  sheetName = "prior of probabilities",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

write.xlsx(
  theta.init1$transmat,
  file="Simulated MSAR model.xlsx",
  sheetName = "transition matrix",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
#3-Simulating a MSAR model
Bsim = 100# can be any large no. at least 100
Ksim = Bsim*N.samples;Ksim
ksimm=100# this is used in Y0 and y.sim insteal of Ksim
# Relancé les simulations juseque le théta est présque le meme comme dans le pre determinations indicé avant 
Y0 = array(Y[1,sample(1:dim(Y)[1],1,replace=T),],c(2,ksimm,1))
Y.sim = simule.nh.MSAR(mod.lynx.hh$theta,Y0 = Y,T,N.samples = ksimm)
Y.sim$S#simulated Markov chain
Y.sim$Y#simulated observation time series
Y.sim[["Y"]]
length(Y.sim[["Y"]])
plot(Y.sim$Y)
lines(data.month)
install.packages("openxlsx")
library(openxlsx)# used to open large files, since this results is x,y matrix with 1629 simulations
write.xlsx(
  Y.sim$S,
  file="Simulated MSAR model.xlsx",
  sheetName = "Simulated Markov Chain",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  Y.sim[["Y"]],
  file="Simulated MSAR model.xlsx",
  sheetName = "Simulated MSAR model Observations",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

####################################################################
# Conditional Probability of  simulated MSAR model
# here the code is for all the simulations and can be changed to the specific simulation by inserting the y[ex]
CP = Cond.prob.MSAR(Y, mod.lynx.hh$theta)
CP$yrange#values at which the conditional probabilities are computed
CP$prob#conditional probabilities for each time t and each values of yrange
CP$Yhat#mode of the conditinal distribution for each time t
plot(CP$prob,typ="l",main="conditional probabilities",xlab="Time",ylab="Captured")

write.xlsx(
  CP$yrange,
  file="Simulated MSAR model.xlsx",
  sheetName = "values at which CP are computed",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)

write.xlsx(
  CP$prob,
  file="Simulated MSAR model.xlsx",
  sheetName = "CP & values at which are are computedat each step  ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  CP$Yhat,
  file="Simulated MSAR model.xlsx",
  sheetName = "mode of the CP distribution  ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)

ex = 1:1656;ex
lex = length(ex);lex
tps = (1883:2020)[ex]
tps
ymsarsim<- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/msarsim.xlsx")
Yex=array(ymsarsim$sim,length(ymsarsim))# array of the chosen simulated msar model

par(mfrow=c(2,1))
# Plot the Conditional probability in yearly
plot(tps,datamsar,typ="l",main="Homogeneous MSAR model",xlab="Time",ylab="Captured")
lines(tps,CP$Yhat,col="red")
alpha = .05
IC.emps = matrix(0,2,lex)
for (k in 1:lex) {
  tmp = cumsum(CP$prob[,k,])/sum(CP$prob[,k,])
  IC.emps[1,k] = CP$yrange[max(c(which(tmp<alpha/2),1))]
  IC.emps[2,k] = CP$yrange[min(max(which(tmp<(1-alpha/2))),length(CP$yrange))]
}
lines(tps,IC.emps[1,],lty=2,col="blue")
lines(tps,IC.emps[2,],lty=2,col="blue")
IC1ys=as.data.frame(IC.emps[1,])
IC2ys=as.data.frame(IC.emps[2,])


# Plot the Conditional probability in monthly
plot(ex,datamsar,typ="l",main="Homogeneous MSAR model",xlab="Time",ylab="Captured")
lines(ex,CP$Yhat,col="red")
alpha = .05
IC.empms = matrix(0,2,lex)
for (k in 1:lex) {
  tmp = cumsum(CP$prob[,k,])/sum(CP$prob[,k,])
  IC.empms[1,k] = CP$yrange[max(c(which(tmp<alpha/2),1))]
  IC.empms[2,k] = CP$yrange[min(max(which(tmp<(1-alpha/2))),length(CP$yrange))]
}
lines(ex,IC.empms[1,],lty=2,col="blue")
lines(ex,IC.empms[2,],lty=2,col="blue")

IC1ms=as.data.frame(IC.empms[1,])
IC2ms=as.data.frame(IC.empms[2,])

CPyearlyandmonthlysimulated=data.frame(CP$Yhat,IC1ys,IC2ys,tps,datamsar,IC1ms,IC2ms)
CPyearlyandmonthlysimulated

write.xlsx(
  CPyearlyandmonthlysimulated,
  file="Simulated MSAR model.xlsx",
  sheetName = " CP plot ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
dfobservedcps = as.data.frame(datamsar)
dfobservedcps
dffitted.R2cps=as.data.frame(CP$Yhat)
dffitted.R2cps

dffitted.R2cptssim=ts(dffitted.R2cps)
dfobservedcptssim=ts(dfobservedcps)
R2cpsim=rsq(glm(dfobservedcptssim~dffitted.R2cptssim),adj = FALSE)
R2cpsim
write.xlsx(
  R2cpsim,
  file="Simulated MSAR model.xlsx",
  sheetName = " R square(mode of distrubution and data) ",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
###Mean Duration of sojourn under a treshold

#Plot the mean duration of sojourn over thresholds for an observed time 
#series and a simulated one with respect to the empirical cumulative 
#distribution function. Fluctuation intervals are plotted too.

#With out logged simulated model
uu = seq(min(Y),max(Y),length.out=10)
Meandurationunder=MeanDurUnder(Y,Y.sim$Y,uu)
Meandurationunder$F
Meandurationunder$mdu.data
Meandurationunder$F.sim
Meandurationunder$mdu.sim
Meandurationunder$CI
Meandurationunder$mdu.sim.all
empiricalcdfofdata=as.data.frame(Meandurationunder$F)
empiricalcdfofsimulation=as.data.frame(Meandurationunder$F.sim)
meandurationbelowlevelsuforsimulations=as.data.frame(Meandurationunder$mdu.sim)
meandurationbelowlevelsufordata=as.data.frame(Meandurationunder$mdu.data)
CIforsimulation=as.data.frame(Meandurationunder$CI)
MeanDurationCDFUnderData=data.frame(empiricalcdfofdata
                                    ,empiricalcdfofsimulation
                                    ,CIforsimulation
)

write.xlsx(
  MeanDurationCDFUnderData,
  file="MeanDurationofsimulatedMSAR.xlsx",
  sheetName = "Empirical CDF",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)

write.xlsx(
  meandurationbelowlevelsuforsimulations,
  file="MeanDurationofsimulatedMSAR.xlsx",
  sheetName = "meandurationbelowlevelsuforsimulations",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  meandurationbelowlevelsufordata,
  file="MeanDurationofsimulatedMSAR.xlsx",
  sheetName = "meandurationbelowlevelsufordata",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
#Performs bootstrap statistical on covariance to validate MSVAR models

bootstrapcovariance=test.model.vect.MSAR(Y,Y.sim$Y,lag=NULL)
bootstrapcovariance$Cvect
bootstrapcovariance[["Cvect"]][["dd"]]#test statistic
bootstrapcovariance[["Cvect"]][["q.dd"]]#quantiles .05 and .95 of the distribution of the test statistic underthe null hypothesis
bootstrapcovariance[["Cvect"]][["p.value"]]
write.xlsx(
  bootstrapcovariance[["Cvect"]][["dd"]],
  file="BootstrapcovariancetestMSARdata&simulated.xlsx",
  sheetName = "Test Statistics",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE)

write.xlsx(
  bootstrapcovariance[["Cvect"]][["q.dd"]],
  file="BootstrapcovariancetestMSARdata&simulated.xlsx",
  sheetName = "distributionoftheteststatisticunderthenullhypothesis",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
write.xlsx(
  bootstrapcovariance[["Cvect"]][["p.value"]],
  file="BootstrapcovariancetestMSARdata&simulated.xlsx",
  sheetName = "Pvalue",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
###Statistics plotting for validation of MSAR models

#plots some functional statistics to help to valid MSAR models: qqplot, covariance function, 
#mean duration of sojourn over and under a threshold. For each of them 
#the empirical statistic of the observed time series is plotted as well
#as the simulated one with (1 ??? ??)-fluctuation intervals.
ValidtestMSAR=valid_all.MSAR(Y,Y.sim$Y,title="",id=1,alpha=.05,spaghetti=TRUE,
                             mfrow=NULL,save=FALSE,output=TRUE,
                             root.filename=" ",path=NULL,col="red",width=4,height=4)
################################################
# Seasonal Markov switching component

##Monthly seasonal effect

# Months of interest
Vart=(v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12)/12
vr1=((v1/Vart)*100)-100
vr2=((v2/Vart)*100)-100
vr3=((v3/Vart)*100)-100
vr4=((v4/Vart)*100)-100
vr5=((v5/Vart)*100)-100
vr6=((v6/Vart)*100)-100
vr7=((v7/Vart)*100)-100
vr8=((v8/Vart)*100)-100
vr9=((v9/Vart)*100)-100
vr10=((v10/Vart)*100)-100
vr11=((v11/Vart)*100)-100
vr12=((v12/Vart)*100)-100

vm=array(data = c(vr1,vr2,vr3,vr4,vr5,vr6,vr7,vr8,vr9,vr10,vr11,vr12))

moi=as.numeric(0)
for (i in 1:12) {
  if (vm[i]>=0) {
    moi[i]=vm[i]
  }
}
moi# months of interest
# Months of interest effect value
Evt1=mean(data.janv)*(100-(janv.ratiov-100))/100
Evt2=mean(data.fev)*(100-(fev.ratiov-100))/100
Evt3=mean(data.mars)*(100-(mars.ratiov-100))/100 # variance ratio is -ve therefore refused
Evt11=mean(data.novembre)*(100-(novembre.ratiov-100))/100  # variance ratio is -ve therefore refused
Evt12=mean(data.decembre)*(100-(decembre.ratiov-100))/100
# Months of interest are jan,feb,mars,novem,decem.
#######################################
# years of seasonality:
#The input used is the yearly variance and the yearly mean value of the dataset
Seasonalitycalculations<- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/conditionalseasonalityinput.xlsx")
date <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/conditionalseasonalityinput.xlsx",range = cell_cols("A") )
yearlymean <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/conditionalseasonalityinput.xlsx",range = cell_cols("B") )
yearlyvariance <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/conditionalseasonalityinput.xlsx",range = cell_cols("C") )
length(yearlyvariance$`Yearly variance`)
#################################
# yearly variance
yv=array(yearlyvariance$`Yearly variance`,dim = 138)
meanyv=mean(yv)#average of the historical yearly variance
jv=var(data.janv)# variance of the month
fv=var(data.fev)# variance of the month
dv=var(data.decembre)# variance of the month
meanyvar=array(data=meanyv,dim = 138)# array of the average value of the historical yearly variance
#################################

Time=data.frame(matrix(c(date$Date), ncol = 1, nrow = 138)) 
Yearlymeanar= data.frame(matrix(c(yearlymean$`Yearly mean`), ncol = 1, nrow = 138))  
Yearlyvariancear=data.frame(matrix(c(yearlyvariance$`Yearly variance`), ncol = 1, nrow = 138))
Yearsofseasonality=data.frame(matrix("", ncol = 1, nrow = 138))
RYV=data.frame(matrix("", ncol = 1, nrow = 138))
MOIef=data.frame(matrix("", ncol = 1, nrow = 138))
MOIefm=data.frame(matrix("", ncol = 1, nrow = 138))
HSMYS=data.frame(Time,Yearlymeanar,Yearlyvariancear,Yearsofseasonality,RYV,MOIefm,MOIef)
colnames(HSMYS) <- c("Time","Yearlymean","Yearlyvariance","SeasonalityDecision","RemainderYearlyVariance","MOIefm","MOIef")
HSMYS
HSMYS$SeasonalityDecision=ifelse(HSMYS$Yearlyvariance>meanyvar,"seasonality","Noseasonality")
HSMYS
HSMYS$RemainderYearlyVariance=ifelse(HSMYS$SeasonalityDecision=="seasonality",yes =  eval(parse(text="(HSMYS$Yearlyvariance-meanyvar)")),no = NA)
HSMYS
HSMYS$MOIefm=ifelse(HSMYS$RemainderYearlyVariance<=dv,yes =c("December"),no = ifelse(HSMYS$RemainderYearlyVariance<=jv,yes =c("January"),no =c("February")))
HSMYS
HSMYS$MOIef=ifelse(HSMYS$RemainderYearlyVariance<=dv,yes =  eval(parse(text="(Evt12)")),no = ifelse(HSMYS$RemainderYearlyVariance<=jv,yes =  eval(parse(text="(Evt1)")),no = eval(parse(text="(Evt2)"))))
HSMYS
#########################################################################

#####Create a hidden Markov model for seasonal probabilistic 
library(HMM)
# Transition probability matrix is calibrated based on the Months distribution effect
HMMS=initHMM(c("January","February","December"),c("January","February","December"),startProbs = matrix(c(1,0,0)),transProbs=matrix(c(0,0.142857,0.069767,0,0.142857,0.139535,1,0.714286,0.790698),3,3))
                                                                                                                             
HMMS.df <- as.data.frame(HMMS)
HMMS.df
HMMS$States#Vector with the names of the states.
HMMS$Symbols#Vector with the names of the symbols.
HMMS$startProbs#Annotated vector with the starting probabilities of the states.
#States transition probabilistic matrix based on the occurance of the Seasonal probabilistic transition matrix
HMMS$transProbs#Annotated matrix containing the transition probabilities between the states.
#Seasonality probabilistic transition matrix
HMMS$emissionProbs#Annotated matrix containing the emission probabilities of the states.
HMMSstates.df=as.data.frame(HMMS$States)
write.xlsx(
  HMMSstates.df,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Months",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
HMMSsymbol.df=as.data.frame(HMMS$Symbols)
write.xlsx(
  HMMSsymbol.df,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Conditions",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
HMMSStartprobs.df=as.data.frame(HMMS$startProbs)
write.xlsx(
  HMMSStartprobs.df,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Start probabilistic",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
HMMStransprobs.df=as.data.frame(HMMS$transProbs)
write.xlsx(
  HMMStransprobs.df,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Transition probabilistic Matrix between months",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
HMMSemissionprobs.df=as.data.frame(HMMS$emissionProbs)
write.xlsx(
  HMMSemissionprobs.df,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Seasonality occurance probability matrix",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)


##Simulates a path of states and observations for a given Hidden Markov Model
HMMSpath=simHMM(HMMS, 55)
HMMSpath$states# States for the seasonality if seasonality is satisfied 
HMMSpath$observation
moiseq=na.omit(data.frame(HSMYS$MOIefm))
rsqhsmv=0
moiseqts=ts(moiseq)
while(rsqhsmv<0.89){
  HMMSpath=simHMM(HMMS, 55)
  HMMSpathts=ts(data.frame(HMMSpath$states))
  calculatedpath=moiseqts
  rsqhsm=glm(HMMSpathts~calculatedpath)
  rsqhsmv=rsq(rsqhsm)
}
rsqhsmvv=rsqhsm[["coefficients"]][["calculatedpath"]]

Markovtransitionstates=as.data.frame(HMMSpathts)
write.xlsx(
  Markovtransitionstates,
  file="Seasonal Markov chain.xlsx",
  sheetName = "Simulated transition states",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
SS=data.frame(matrix("Markovtransitionstates", ncol = 1, nrow = 55))
SMOIev=data.frame(matrix("", ncol = 1, nrow = 55))

HSMYSS=data.frame(SS,SMOIev)
colnames(HSMYSS) <- c("Simulated Seasonal Markov chain","MOISS")
HSMYSS$SMOIev=ifelse(HSMYSS$SS=="January",Evt1,ifelse(HSMYSS$SS=="February",Evt2,ifelse(HSMYSS$SS=="December",Evt12,NA)))

# the calculated years of seasonality considering the months of interest effect value is then added to the simulated MSAR model in excel or as follows in the code, however, this part of addition in the code has not been launched and checked so if it did not work: just add the HSM value to the MSAR values only when the state of time is considerd as a year of seasonality

HSMarray= array((datamsar),c(length(datamsar),1,1))

seasonality_years=array(HSMYS$Yearsofseasonality)#Years of seasonality
# Replicate the years of seasonalty by 12 to represent the monthly distribution in order to embed a loop for the same length 
seasonalityrep=replicate(12,seasonality_years)
seasonalityarray=array(seasonalityrep)
colnames(seasonalityarray)<- c("YOS")
HMMSpatharr=array(HMMSpathts)
HMMSpatharrrep=replicate(length(seasonalityrep)/55,HMMSpatharr)# to give the same length to be able to loop it
colnames(HMMSpatharrrep) <- c("Month")

SMSARFill=array(matrix("", ncol = 1, nrow = length(HSMarray)))

for (i in seasonalityrep) {
    SMSARFill[i] <- ifelse(HMMSpatharrrep[i,"Month"] == "January" & seasonalityarray["YOS"]=="seasonality", HSMarray[i]+Evt1,
                                       ifelse(HMMSpatharrrep[i,"Month"] == "February"& seasonalityarray["YOS"]=="seasonality", HSMarray[i]+Evt2,
                                              ifelse(HMMSpatharrrep[i,"Month"] == "December"& seasonalityarray["YOS"]=="seasonality", HSMarray[i]+Evt3, HSMarray[i])))
  }
SMSARFill# SMSAR fitted model
write.xlsx(
  SMSARFill,
  file="SMSAR.xlsx",
  sheetName = "SMSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE,
  showNA = TRUE
)
########################################################################
###########################################
#### MSGARCH

#1-Create a spec:

#Creates a model specification before fitting and using the MSGARCH functionalities

#CreateSpec(
#variance.spec = list(model = c("sGARCH", "sGARCH")),
#distribution.spec = list(distribution = c("norm", "norm")),
#switch.spec = list(do.mix = FALSE, K = NULL),
#constraint.spec = list(fixed = list(), regime.const = NULL),
#prior = list(mean = list(), sd = list())
#)
#mspec=CreateSpec(
#  variance.spec = list(model = c("sGARCH")),
# distribution.spec = list(distribution = c( "norm")),
# switch.spec = list(do.mix = TRUE, K = NULL),
# constraint.spec = list(fixed = list(), regime.const = NULL),
# prior = list(mean = list(), sd = list())
#)
#print(mspec)
#mspec$par0#Vector (of size d) of default parameters
#mspec$is.mix#Logical indicating if the specification is a mixture
#mspec$K#Number of regimes
#mspec$lower# Vector (of size d) of lower parameters' bounds
#mspec$upper# Vector (of size d) of upper parameters' bounds
#mspec$n.params#Vector (of size K) of the total number of parameters by regime including distributions' parameters.
#mspec$n.params.vol#Vector (of size K) of the total number of parameters by regime excluding distributions' parameters.
#mspec$label#Vector (of size d) of parameters' labels
#mspec$name#Vector (of size K) of model specifications' names
#mspec$prior.mean
#mspec$prior.sd
#mspec$fixed.pars.bool
#mspec$regime.const.pars.bool

#######################################################
# Severe values
setwd("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results")

#######################################
# Reading (opening) the data file and choosing specific data from it

File<- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/severevaluesinput.xlsx")
Time <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/severevaluesinput.xlsx",range = cell_cols("A") )
original <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/severevaluesinput.xlsx",range = cell_cols("B") )
msar <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/severevaluesinput.xlsx",range = cell_cols("C") )
smsar <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/severevaluesinput.xlsx",range = cell_cols("D") )
#######################################
##Extracrtion
#original.dataframe[original.dataframe>=250]
#filter(original.dataframe,original>=250)
#original.dataframe %>% filter(original >=250)
Time.dataframe=data.frame(Time)
original.dataframe=data.frame(original)
msar.dataframe=data.frame(msar)
smsar.dataframe=data.frame(smsar)

#Originalseverevalues250
#severevalues
original250=filter(original.dataframe,original>=260);original250
#Index
original.dataframeindex250 <- which(original.dataframe$original >= 260);original.dataframeindex250
original.dataframeindex250.df=data.frame(original.dataframeindex250);original.dataframeindex250.df
#Date based on the Index
Time.dataframe250=Time.dataframe$Date[original.dataframeindex250];Time.dataframe250
original.dataframe250=original.dataframe$original[original.dataframeindex250];original.dataframe250

#Creating the severe File
Originalseverevalue250=data.frame(Time.dataframe250,original.dataframe250)
colnames(Originalseverevalue250) <- c("Date", "original250");Originalseverevalue250
#Saving
write.xlsx(
  Originalseverevalue250,
  file="SevereValues260.xlsx",
  sheetName = "Original",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)
#SMSAR severe values extraction
smsar250=filter(smsar.dataframe,smsar>=260);smsar250
#Index
smsar.dataframeindex250 <- which(smsar.dataframe >= 260);smsar.dataframeindex250
smsar.dataframeindex250.df=data.frame(smsar.dataframeindex250);smsar.dataframeindex250.df
#Date based on the Index
Time.dataframe250=Time.dataframe$Date[smsar.dataframeindex250];Time.dataframe250
smsar.dataframe250=smsar.dataframe$smsar[smsar.dataframeindex250];smsar.dataframe250

#Creating the severe File
smsarseverevalue250=data.frame(Time.dataframe250,smsar.dataframe250)
colnames(smsarseverevalue250) <- c("Date", "Smsar250");smsarseverevalue250
#Saving
write.xlsx(
  smsarseverevalue250,
  file="SevereValues260.xlsx",
  sheetName = "SMSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
#MSAR Severe values extraction
msar250=filter(msar.dataframe,msar>=260);msar250
#Index
msar.dataframeindex250 <- which(msar.dataframe >= 260);msar.dataframeindex250
msar.dataframeindex250.df=data.frame(msar.dataframeindex250);msar.dataframeindex250.df
#Date based on the Index
Time.dataframe250=Time.dataframe$Date[msar.dataframeindex250];Time.dataframe250
msar.dataframe250=msar.dataframe$msar[msar.dataframeindex250];msar.dataframe250

#Creating the severe File
msarseverevalue250=data.frame(Time.dataframe250,msar.dataframe250)
colnames(msarseverevalue250) <- c("Date", "Msar260");msarseverevalue250
#Saving
write.xlsx(
  msarseverevalue250,
  file="SevereValues260.xlsx",
  sheetName = "MSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
# kernel probability distribution
#Histogram and density function
#freqhisto=hist(original$original, n = 60, freq = TRUE, border = "white", col = "steelblue")
#densityhisto=hist(original$original, n = 60, freq = FALSE, border = "white", col = "steelblue")
#freqhisto$density#density on y axis
#freqhisto$breaks#intervals on x axis 
#freqhisto$counts#frequency on y axis
do=density(original$original,n=60,from = 0,to = 430);plot(do)
do$x
do$y# density
denistyframeoriginal=data.frame(do$x,do$y)
colnames(denistyframeoriginal) <- c("Xaxis","density")
write.xlsx(
  denistyframeoriginal,
  file="Kerneldistribution.xlsx",
  sheetName = "Dataset",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
#frequencyframeoriginal=data.frame(freqhisto$counts,freqhisto$density)
#colnames(frequencyframeoriginal) <- c("frequency","density")
#frequencyframexaxisoriginal=data.frame(freqhisto$breaks)
#colnames(frequencyframexaxisoriginal) <- c("X")
meanoriginal=mean(original$original)
medianoriginal=median(original$original)
sd(original$original)
cv <- sd(original$original) / mean(original$original) * 100
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v=original$original
modeoriginal=getmode(v);modeoriginal
statisticaloriginal=data.frame(meanoriginal,medianoriginal,modeoriginal)
colnames(statisticaloriginal) <- c("mean","median","mode")
#MSAR Kernel density 
dm=density(msar$msar,n=60,from = 0,to = 430);plot(dm)
dm$x
dm$y# density
denistyframemsar=data.frame(dm$x,dm$y)
colnames(denistyframemsar) <- c("Xaxis","density")
write.xlsx(
  denistyframemsar,
  file="Kerneldistribution.xlsx",
  sheetName = "MSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
#frequencyframeoriginal=data.frame(freqhisto$counts,freqhisto$density)
#colnames(frequencyframeoriginal) <- c("frequency","density")
#frequencyframexaxisoriginal=data.frame(freqhisto$breaks)
#colnames(frequencyframexaxisoriginal) <- c("X")
msarmean=mean(msar$msar)
medianmsar=median(msar$msar)
sd(msar$msar)
cv <- sd(msar$msar) / mean(msar$msar) * 100


getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
x=msar$msar
modemsar=getmode(x)
statisticalmsar=data.frame(msarmean,medianmsar,modemsar)
colnames(statisticalmsar) <- c("mean","median","mode")
#SMSAR Kernel density 
ds=density(smsar$smsar,n=60,from = 0,to = 430);plot(ds)
ds$x
ds$y# density
denistyframesmsar=data.frame(ds$x,ds$y)
colnames(denistyframesmsar) <- c("Xaxis","density")
write.xlsx(
  denistyframesmsar,
  file="Kerneldistribution.xlsx",
  sheetName = "SMSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
#frequencyframeoriginal=data.frame(freqhisto$counts,freqhisto$density)
#colnames(frequencyframeoriginal) <- c("frequency","density")
#frequencyframexaxisoriginal=data.frame(freqhisto$breaks)
#colnames(frequencyframexaxisoriginal) <- c("X")
smsarmean=mean(smsar$smsar)
mediansmsar=median(smsar$smsar)
sd(smsar$smsar)
cv <- sd(smsar$smsar) / mean(smsar$smsar) * 100
getmode <- function(w) {
  uniqv <- unique(w)
  uniqv[which.max(tabulate(match(w, uniqv)))]
}
w=smsar$smsar
modesmsar=getmode(w)
statisticalsmsar=data.frame(smsarmean,mediansmsar,modesmsar)
colnames(statisticalsmsar) <- c("mean","median","mode")


#######################################################################
###Plots empirical expected number of upcrossings level u with P(Y<u)

#ENu_graph function Plots empirical expected number of upcrossings of 
#level u with respect to P(Y<u)

# Working on Database with log transform

uo= seq(min(original$original),max(original$original),by=10)
um= seq(min(msar$msar),max(msar$msar),by=10)
us= seq(min(smsar$smsar),max(smsar$smsar),by=10)
#Choose bounds of the input and write the u function as a sequence based 
#on the sequential distribution of the dataset, and the simulations 
u=seq(0,430,by=10)

# Dataset empirical expected number of upcrossings level
EENUO = ENu_graph(ts(original),u)#probability of data<ulog
EENUO$u#sequence of levels
EENUO$F#empirical cdf: P(data<u)
EENUO$Nu#number of upcrossings
EENUO$CI#fluctuation interval
EENUOdf=as.data.frame(EENUO)
EENUOdf
##################################

# MSAR empirical expected number of upcrossings level
EENUM = ENu_graph(ts(msar),u)#probability of data<ulog
EENUM$u#sequence of levels
EENUM$F#empirical cdf: P(data<u)
EENUM$Nu#number of upcrossings
EENUM$CI#fluctuation interval
EENUMdf=as.data.frame(EENUM)
EENUMdf

##################################

# SMSAR empirical expected number of upcrossings level
EENUS = ENu_graph(ts(smsar),u)#probability of data<ulog
EENUS$u#sequence of levels
EENUS$F#empirical cdf: P(data<u)
EENUS$Nu#number of upcrossings
EENUS$CI#fluctuation interval
EENUSdf=as.data.frame(EENUS)
EENUSdf

write.xlsx(
  EENUOdf,
  file="numberofupcrossing.xlsx",
  sheetName = "Dataset",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  EENUMdf,
  file="numberofupcrossing.xlsx",
  sheetName = "MSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  EENUSdf,
  file="numberofupcrossing.xlsx",
  sheetName = "SMSAR",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
####################################
# Neural Network forecasting
setwd("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results")

#######################################
# Reading (opening) the data file and choosing specific data from it

File<- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx")
dataset10 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("B") )
msar10 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("C") )
smsar10 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("D") )
dataset20 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("F") )
msar20 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("G") )
smsar20 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("H") )
dataset40 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("J") )
msar40 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("K") )
smsar40 <- read_excel("C:/Users/BASSEL/Desktop/PHD third year/Previous Work/Long memory process/SMSAR Model/SMSAR Model using R/results/analysis/NNinput1930.xlsx",range = cell_cols("L") )
#######################################

#Monthly MSAR Neural Network

#Fitting the model

datasetneuralmonthly=nnetar(ts(dataset10$dataset10),p=11,P=c(1))
datasetneuralmonthly$fitted
datasetneuralmonthly$residuals
datasetneuralmonthly$method#(x;y) 2 hidden layers each one has the value of neurons wrtten
datasetneuralmonthly$model # model information
summary(datasetneuralmonthly)

# Forecasting the NN
datasetneuralmonthlyforecast=forecast(datasetneuralmonthly, h=60)
plot(datasetneuralmonthlyforecast)

datasetneuralmonthlyforecastdf=data.frame(datasetneuralmonthlyforecast)
# Accuracy
datasetneuralmonthlyaccuracy=accuracy(datasetneuralmonthlyforecast);datasetneuralmonthlyaccuracy
datasetneuralmonthly.accuracydf=data.frame(datasetneuralmonthlyaccuracy)
# Dataframe of the results
datasetneuralmonthly.fitteddf=data.frame(datasetneuralmonthly$fitted)
datasetneuralmonthly.residualsdf=data.frame(datasetneuralmonthly$residuals)
datasetneuralmonthly.methoddf=data.frame(datasetneuralmonthly$method)
datasetneuralmonthly$model
datasetneuralmonthly.modeldf=data.frame("Average of 20 networks, each of which is
a 11-6-1 network with 79 weights
options were - linear output units ")
# Saving the results
write.xlsx(
  datasetneuralmonthly.fitteddf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Fitted Model",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)
write.xlsx(
  datasetneuralmonthly.residualsdf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Residuals",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  datasetneuralmonthly.methoddf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Method",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  datasetneuralmonthly.modeldf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Model",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  datasetneuralmonthlyforecastdf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Forecast",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  datasetneuralmonthly.accuracydf,
  file="SMSAR40-Monthly.xlsx",
  sheetName = "Accuracy",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)

#3 Yearly effect trial MSAR Neural network
msarneuralyearly=nnetar(ts(smsar40$smsar40),p=35,P=c(1))
msarneuralyearly$fitted
msarneuralyearly$residuals
msarneuralyearly$method#(x;y) 2 hidden layers each one has the value of neurons wrtten
msarneuralyearly$model # model information
summary(msarneuralyearly)
msarneuralyearlyforecast=forecast(msarneuralyearly, h=60)
plot(msarneuralyearlyforecast)
msarneuralyearlyaccuracy=accuracy(msarneuralyearlyforecast);msarneuralyearlyaccuracy

msarneuralmonthly.fitteddfyearly=data.frame(msarneuralyearly$fitted)
msarneuralmonthly.residualsdfyearly=data.frame(msarneuralyearly$residuals)
msarneuralmonthly.methoddfyearly=data.frame(msarneuralyearly$method)
msarneuralmonthly.modeldfyearly=data.frame("Average of 20 networks, each of which is
a 35-18-1 network with 667 weights
options were - linear output units")
msarneuralmonthlyforecastdfyearly=data.frame(msarneuralyearlyforecast)
msarneuralmonthly.accuracydfyearly=data.frame(msarneuralyearlyaccuracy)
#Saving
write.xlsx(
  msarneuralmonthly.fitteddfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Fitted Model",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)
write.xlsx(
  msarneuralmonthly.residualsdfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Residuals",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.methoddfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Method",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.modeldfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Model",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthlyforecastdfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Forecast",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.accuracydfyearly,
  file="SMSAR40-yearly.xlsx",
  sheetName = "Accuracy",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
# Auto simulation NN (Not recommended)
#Yearly MSAR Neural network

msarneuralyearly=nnetar(ts(smsar50$smsar50),repeats = 100)
msarneuralyearly$fitted
msarneuralyearly$residuals
msarneuralyearly$method#(x;y) 2 hidden layers each one has the value of neurons wrtten
msarneuralyearly$model # model information
summary(msarneuralyearly)
msarneuralyearlyforecast=forecast(msarneuralyearly, h=84)
plot(msarneuralyearlyforecast)
msarneuralyearlyaccuracy=accuracy(msarneuralyearlyforecast);msarneuralyearlyaccuracy

msarneuralmonthly.fitteddfyearly=data.frame(msarneuralyearly$fitted)
msarneuralmonthly.residualsdfyearly=data.frame(msarneuralyearly$residuals)
msarneuralmonthly.methoddfyearly=data.frame(msarneuralyearly$method)
msarneuralyearly$model # model information
msarneuralmonthly.modeldfyearly=data.frame("Average of 100 networks, each of which is
a 27-14-1 network with 407 weights
options were - linear output units")
msarneuralmonthlyforecastdfyearly=data.frame(msarneuralyearlyforecast)
msarneuralmonthly.accuracydfyearly=data.frame(msarneuralyearlyaccuracy)
#Saving
write.xlsx(
  msarneuralmonthly.fitteddfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Fitted Model",
  col.names = TRUE,
  row.names = TRUE,
  append = FALSE
)
write.xlsx(
  msarneuralmonthly.residualsdfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Residuals",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.methoddfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Method",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.modeldfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Model",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthlyforecastdfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Forecast",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)
write.xlsx(
  msarneuralmonthly.accuracydfyearly,
  file="SMSAR50-AUTO.xlsx",
  sheetName = "Accuracy",
  col.names = TRUE,
  row.names = TRUE,
  append = TRUE
)

