library(TSstudio)
library(readxl)
library(forecast)
library(tidyverse)
library(dplyr)


# Read dataset
df1 <- read_excel("/Users/maido/Documents/Data Analytics MSc OXBR 2020/Time Series Analysis DALT 7010/Assessments/Mai Do.xlsx", col_names = FALSE)
df1 = df1 %>% rename(value=...1)
str(df1)

# Convert to TS
ts <- ts(df1, start = c(2010,1), frequency = 12)
ts_info(ts)

##### a #####
# Plot the time series
autoplot(ts)

df <- mutate(df1, time=seq(1,108))
attach(df)

# The model
exp.model <- lm(log(value)~time,data = df) # Logistic

exp.model.df <- data.frame(x=time, y=exp(fitted(exp.model))) # Exponential

# Plot
ggplot(df, aes(x = time, y = value)) + geom_line() +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'quadratic'), se = FALSE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,3), aes(colour = 'cubic'), se = FALSE)+
  stat_smooth(data=exp.model.df, method = 'loess',aes(colour = 'exponential'), se = FALSE) 


# Linear model 3094 - 3725.491
linear <- lm(value~time)
#summary(linear)
CV(linear)


# Exponential model => biggest 3347. value = 9.2*(0.006^time) 0.2623364
exponential <- lm(log(value)~time)
#summary(exponential)
CV(exponential)


# Quadratic model 3133 
quadratic <-lm(value ~ time + I(time^2))
#summary(quadratic)
CV(quadratic)

# stepAIC(quadratic)

# Cubic polynomial model 3133 
cubic <- lm(value ~ time + I(time^2) + I(time^3))
#summary(cubic)
CV(cubic)



##### b #####
# Use ets identify “Additive”  or “Multiplicative”
ets <- ets(ts)
ets
autoplot(ets)
summary(ets)


# Applying Holt- winter’s additive method
fit <- hw(ts, seasonal = "additive")
fitted(fit)

# Plotting the smoothed data
plot.ts(ts, main = "Smooth the Time Series", col = "blue")
lines(fitted(fit),col = "red")

# Estimates of model parameters
fit$model



##### c #####
# Using the estimate of the seasonal component calculated 
decompose_ts <- decompose(ts, "additive")
ts_adj <- ts - decompose_ts$seasonal

# Plot the seasonally adjusted time series
plot(ts_adj)


# Fit a predictive model 
ts_fore <- HoltWinters(ts_adj)
ts_fore


# Plot the adjusted time series 
plot(ts_fore)

# Make forecast
ts_fore2 <- forecast:::forecast.HoltWinters(ts_fore, h=12)
forecast:::plot.forecast(ts_fore2)
summary(ts_fore2)

# The forecast accuracy measures 
accuracy(ts_fore2)

# The Ljung-Box test
acf(ts_fore2$residuals, na.action = na.pass, lag.max=20)
Box.test(ts_fore2$residuals, lag=20, type="Ljung-Box")


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


# Time Plot
plot.ts(ts_fore2$residuals)  

# Histogram
plotForecastErrors(ts_fore2$residuals) 












