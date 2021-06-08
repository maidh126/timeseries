library(readxl)
library(forecast)
library(dplyr)
library(ggplot2)
library(aTSA)
library(urca)

# Read dataset
df <- read_excel("/Users/maido/Documents/Data Analytics MSc OXBR 2020/Time Series Analysis DALT 7010/Assessments/Mai Do.xlsx", col_names = FALSE)
df <- df %>% rename(value=...1)
str(df)


## a
# Plot time series
ts = ts(df, start = c(2010,1), frequency = 12) 
autoplot(ts)


# Check the acf for further inspection
a <- Acf(ts,lag=50)
autoplot(a)


# Ljung-Box test for independence
Box.test(ts, lag=50, type="Ljung-Box")


# Augmented Dickey–Fuller test
stationary.test(ts) 

# KPSS for level or trend stationarity
stationary.test(ts, method = "kpss")


## b
# plot seasonal differencing
cbind("Original" = ts,
      "Transformation" = log(ts),
      "Differencing" = diff(log(ts),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("")

# test
ts %>% log() %>% diff() %>% ur.kpss() %>% summary()


# seasonal differences D=1
nsdiffs(ts) 


## c
# log and diff
ts12 <- diff(log(ts), lag = 12)
autoplot(ts12)
# d = 0, D = 1, s = 12

# Check acf and pacf
acf1 <- Acf(ts12, lag=50)
autoplot(acf1)
pacf1 <- Pacf(ts12, lag=50)
autoplot(pacf1)


## d 
# SARIMA(1,0,1)(0,1,1)12
# sarima(ts, 1,0,1,0,1,1,12)
fit1 <- Arima(ts, order=c(1,0,1), seasonal=c(0,1,1))


# SARIMA(1,0,1)(1,1,1)12
# sarima(ts, 1,0,1,1,1,1,12)
fit2 <- Arima(ts, order=c(1,0,1), seasonal=c(1,1,1))


# auto.arima
fitauto <- auto.arima(ts, test ="kpss")
summary(fitauto)
checkresiduals(fitauto)


## e
# show all the options
auto.arima(ts, trace = TRUE, test ="kpss")


## f
# forecast
# sarima.for(ts, 12, 1,0,1,0,1,1,12)
forecast <- forecast(fitauto, 12)

# Test
acf(forecast$residuals, lag.max=20)
Box.test(forecast$residuals, lag=20, type="Ljung-Box")

# The forecast accuracy measures
accuracy(forecast)

# Function to check whether forecast errors are normally distributed with mean zero
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm=TRUE)/4
  mysd   <- sd(forecasterrors,na.rm=TRUE)
  mymin  <- min(forecasterrors,na.rm=TRUE) - mysd*5
  mymax  <- max(forecasterrors,na.rm=TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}



# Time plot
plot.ts(forecast$residuals)
# Histogram
plotForecastErrors(forecast$residuals)


mean(forecast$residuals)



