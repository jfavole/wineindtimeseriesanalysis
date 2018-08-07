##########################################################################
## Time series analysis: wineind dataset
## Data set measures total bottled wine sales
## from Australian winemakers, 1/1980-8/1994
##########################################################################

##########################################################################
## Import packages and attach dataset
##########################################################################
library(forecast)
library(tseries)
library(astsa)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(seasonal)
library(urca)


##########################################################################
## Exploratory data analysis (EDA)
##########################################################################
str(wineind) # Describes data ending in 1995.
length(wineind)
summary(wineind)

ts(wineind, frequency=12) # Send to a monthly time series object

## Basic plotting

# Paradis sample
tsdisplay(x=wineind,
          cex.lab=1.5,
          cex.main=1.5,
          cex=1.5)

# Standard ggplot
autoplot(wineind) +
  ggtitle("Total monthly Australian wine sales, 1980-1994") +
  theme_light() +
  xlab("Date") +
  ylab("Sales, 1-liter+ bottles")

# Standard ggplot
ggseasonplot(wineind,
             year.labels = TRUE,
             year.labels.left = TRUE) +
  ggtitle("Seasonal plot, monthly Australian wine sales, 1980-1994") +
  theme_light() +
  ylab("Sales, 1-liter+ bottles")

ggseasonplot(wineind,
             polar = TRUE) +
  ggtitle("Polar seasonal plot,\nmonthly Australian wine sales, 1980-1994") +
  theme_light() +
  ylab("Sales, 1-liter+ bottles")

ggsubseriesplot(wineind) +
  ggtitle("Seasonal subseries plot, \nmonthly Australian wine sales, 1980-1994") +
  theme_light() +
  ylab("Sales, 1-liter+ bottles")

## Lag plots
wineind2 <- window(wineind, start=1980, end=c(1994,8))
gglagplot(wineind2) +
  ggtitle("Lag plot, monthly Australian wine sales, 1980-1994") +
  theme_light()

## Correlogram, original data
ggAcf(wineind, lag=48) +
  ggtitle("Autocorrelation plot (correlogram),\nmonthly Australian wine sales, 1980-1994") +
  theme_light()

## Correlogram, differenced data
diffwineind <- diff(wineind)
ggAcf(diffwineind, lag=48) +
  ggtitle("Correlogram, \ndifferenced monthly Australian wine sales, 1980-1994") +
  theme_light()

# PACF, original data
pacf(wineind)

# PACF, differenced data
pacf(diffwineind)

# STL (Seasonal and Trend using Loess) decomposition
wineind %>%
  stl(t.window=13,
      s.window="periodic",
      robust=TRUE) %>%
  autoplot()

# Paradis sample
parts.add <- decompose(x=wineind,
                       type='additive')
plot(x=parts.add,
     cex.lab=1.5,
     cex.main=1.5,
     cex=1.5)

parts.mult <- decompose(x=wineind,
                        type='multiplicative')
plot(x=parts.mult,
     cex.lab=1.5,
     cex.main=1.5,
     cex=1.5)

##########################################################################
## Basic forecasting
##########################################################################

## Mean, naive, seasonal naive (use windowed data)
autoplot(wineind2) +
  autolayer(meanf(wineind2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(wineind2, h=11),
            series="Naive", PI=FALSE) +
  autolayer(snaive(wineind2, h=11),
            series="Seasonal naive", PI=FALSE) +
  ggtitle("Basic forecasting methods,\nmonthly Australian wine sales, 1980-1994") +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottles") +
  theme_light()

##########################################################################
## Tests for stationarity
##########################################################################

## Augmented Dickey-Fuller
## Paradis sample, default formula for k.
## Presence of unit root is the NULL hypothesis.
## More negative D-F more strongly rejects null hypothesis of unit root.

adf.test(x=wineind, alternative='stationary',
         k=trunc((length(wineind)-1)^(1/3)))


adf.test(x=wineind, alternative='stationary', k=12)


## Kwiatkowski-Phillips-Schmidt-Shin
## Presence of unit root is the ALTERNATIVE hypothesis.
kpss.test(wineind, null='Level', lshort=T)
kpss.test(wineind, null='Level', lshort=F)

##########################################################################
## Transformations and adjustments
##########################################################################

## Box-Cox transformation
(lambda <- BoxCox.lambda(wineind))
autoplot(BoxCox(wineind, lambda)) +
  ggtitle("Box-Cox transformed monthly Australian wine sales, 1980-1994") +
  theme_light()

## Back transformed and bias-adjusted forecasts
fc <- rwf(wineind, drift=TRUE, lambda=1, h=50, level=80)
fc2 <- rwf(wineind, drift=TRUE, lambda=0.5, h=50, level=80, biasadj=TRUE)

autoplot(wineind) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(color=guide_legend(title="Forecast")) +
  ggtitle("Transformed and adjusted forecasts") +
  theme_light()

##########################################################################
## Residual diagnostics
## All indicate autocorrelation
##########################################################################

## Naive methods
res_naive <- residuals(naive(wineind))
autoplot(res_naive) +
  xlab("Time") +
  ylab("") +
  ggtitle("wineind: Residuals from naive forecasting") +
  theme_light()

gghistogram(res_naive) +
  ggtitle("wineind: Histogram of residuals from naive forecasting") +
  theme_light()

ggAcf(res_naive) +
  ggtitle("wineind: ACF of residuals from naive forecasting") +
  theme_light()

## Box-Pierce test
Box.test(res_naive, lag=10, fitdf=0)

## Box-Ljung test
Box.test(res_naive, lag=10, fitdf=0, type="Lj")

##########################################################################
## Moving average
##########################################################################

autoplot(wineind, series="Data") +
  autolayer(ma(wineind, 3), series="3-MA") +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottle") +
  ggtitle("3-period moving average, wineind") +
  scale_color_manual(values=c("Data"="lightgrey", "3-MA"="orange"),
                     breaks=c("Data", "3-MA")) +
  theme_light()

autoplot(wineind, series="Data") +
  autolayer(ma(wineind, 6), series="6-MA") +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottle") +
  ggtitle("6-period moving average, wineind") +
  scale_color_manual(values=c("Data"="lightgrey", "6-MA"="green"),
                     breaks=c("Data", "6-MA")) +
  theme_light()

autoplot(wineind, series="Data") +
  autolayer(ma(wineind, 12), series="12-MA") +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottle") +
  ggtitle("12-period moving average, wineind") +
  scale_color_manual(values=c("Data"="lightgrey", "12-MA"="blue"),
                     breaks=c("Data", "12-MA")) +
  theme_light()

##########################################################################
## Decomposition
##########################################################################

## Additive
wineind %>%
  decompose(type="additive") %>%
  autoplot() + 
  xlab("Time") +
  ggtitle("Classical additive decomposition of wineind") +
  theme_light()

## Multiplicative
wineind %>%
  decompose(type="multiplicative") %>%
  autoplot() + 
  xlab("Time") +
  ggtitle("Classical multiplicative decomposition of wineind") +
  theme_light()

## X11
wineind %>%
  seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of wineind") +
  theme_light()

## Combined plot, X11 decomposition
autoplot(wineind, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally adjusted") +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottles") +
  ggtitle("Monthly Australian wine sales, 1980-1994") +
  scale_color_manual(values=c("lightgrey", "lightblue", "orange"),
                     breaks=c("Data", "Seasonally adjusted", "Trend")) +
  theme_light()

## Seasonal sub-series plot, X11 decomposition
fit %>%
  seasonal() %>%
  ggsubseriesplot() +
  ylab("Seasonal") +
  theme_light()

##########################################################################
## Forecasting with decomposition
##########################################################################

## Naive, seasonally adjusted data
fit <- stl(wineind, t.window=13, s.window="periodic", robust=TRUE)
fit %>% seasadj() %>% naive() %>% 
  autoplot() + ylab("Sales, 1-liter+ bottles") +
  ggtitle("Naive forecast, seasonally adjusted wineind data") +
  theme_light()

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

# Paradis sample
parts.stl <- stl(x=wineind, s.window='periodic')
parts.stl.sa <- seasadj(object=parts.stl)

plot(x=wineind,
     type='l',
     main='wineind original',
     cex.lab=1.5,
     cex.main=1.5,
     cex=1.5)

plot(x=parts.stl.sa,
     type='l',
     main='wineind season adjusted',
     cex.lab=1.5,
     cex.main=1.5,
     cex=1.5)

seasonplot(x=parts.stl.sa,
           s=12,
           col=rainbow(12),
           year.labels=T,
           main='wineind seasonal plot',
           cex.lab=2)

##########################################################################
## Exponential smoothing
##########################################################################

## Simple
autoplot(wineind2) +
  ggtitle("Windowed wineind data") +
  ylab("Sales, 1-liter+ bottles") +
  xlab("Year") +
  theme_light()

fc <- ses(wineind2, h=5)
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Sales, 1-liter+ bottles") + xlab("Year") +
  ggtitle("Simple exponential smoothing, wineind") +
  theme_light()

## Holt linear trend and damped trend
fc <- holt(wineind2, h=15)
fc2 <- holt(wineind2, damped=TRUE, phi=0.9, h=15)
autoplot(wineind2) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Sales, 1-liter+ bottles") +
  guides(colour=guide_legend(title="Forecast"))

## Compare exponential smoothing methods
e1 <- tsCV(wineind2, ses, h=1)
e2 <- tsCV(wineind2, holt, h=1)
e3 <- tsCV(wineind2, holt, damped=TRUE, h=1)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

## Holt-Winters seasonal method
fit1 <- hw(wineind2,seasonal="additive")
fit2 <- hw(wineind2,seasonal="multiplicative")
fit3 <- hw(wineind2, damped=TRUE, seasonal="additive")
fit4 <- hw(wineind2, damped=TRUE, seasonal="multiplicative")
autoplot(wineind2) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  autolayer(fit3, series="HW damped additive forecasts", PI=FALSE) +
  autolayer(fit4, series="HW damped multiplicative forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Sales, 1-liter+ bottles") +
  ggtitle("Monthly Australian wine sales") +
  guides(colour=guide_legend(title="Forecast"))

cbind("Sales" = wineind,
      "Monthly log sales" = log(wineind),
      "Annual change \nin log sales" = diff(log(wineind),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly Australian wine sales")

cbind("Sales" = wineind,
      "Logs" = log(wineind),
      "Seasonally\n differenced logs" =
        diff(log(wineind),12),
      "Doubly\n differenced logs" =
        diff(diff(log(wineind),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly Australian wine sales")

## Check ACF and PACF for doubly-differenced data
ddiff <- diff(diff(log(wineind), 12), 1)
ggAcf(ddiff) + theme_light()
pacf(ddiff)

## Unit root test to see if differencing is required
wineind %>% ur.kpss() %>% summary() # Original data non-stationary
diffwineind %>% ur.kpss() %>% summary() # Differenced data stationary
ndiffs(wineind)

## Test whether seasonal differencing is required
wineind %>% log() %>% nsdiffs() 
wineind %>% log() %>% diff(lag=12) %>% ndiffs()

## Differencing causes problems with interpretability;
## need to come back to this.

##########################################################################
## ARIMA modeling
##########################################################################

## Paradis sample: auto.arima

fit.arima <- auto.arima(y=wineind)
checkresiduals(fit.arima)
summary(fit.arima)
str(fit.arima)
accuracy(fit.arima)

qqnorm(
  residuals(fit.arima), 
  cex.lab = 1.5, 
  cex.main = 1.5, 
  cex = 1.5, 
  main = 'Auto ARIMA model residuals normal Q-Q plot'
)
qqline(
  residuals(fit.arima)
)

forecast(object=fit.arima, h=10)
plot(forecast(object=fit.arima, h=10),
     cex.lab=1.5,
     cex.main=1.5,
     cex = 1.5)

##########################################################################
## GARCH modeling
##########################################################################

## Paradis sample
fit.garch <- garch(x=wineind,
                   grad='numerical',
                   trace=F)
summary(fit.garch)
