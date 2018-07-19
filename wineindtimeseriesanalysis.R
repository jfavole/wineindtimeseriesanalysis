##########################################################################
## Time series analysis: wineind dataset
## Data set measures total bottled wine sales
## from Australian winemakers, 1/1980-8/1994
##########################################################################

##########################################################################
## Import packages and attach dataset
##########################################################################
library(forecast)
library(astsa)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(seasonal)
library(urca)
attach(wineind)


##########################################################################
## Exploratory data analysis (EDA)
##########################################################################
str(wineind) # Describes data ending in 1995.
length(wineind)
summary(wineind)

ts(wineind, frequency=12) # Send to a monthly time series object

## Basic plotting
autoplot(wineind) +
  ggtitle("Total monthly Australian wine sales, 1980-1994") +
  theme_light() +
  xlab("Date") +
  ylab("Sales, 1-liter+ bottles")

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

## SEATS decomposition returns an error

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

##########################################################################
## ARIMA modeling
##########################################################################

# 1. Seasonal adjustment of the data
wineind %>% stl(s.window='periodic') %>% seasadj() -> wiadj
autoplot(wiadj) + 
  ggtitle("Seasonally adjusted wineind data") +
  theme_light()

# 2. Residual diagnostics and plots indicate that variance changes,
#    so apply Box-Cox transformation
lambda <- BoxCox.lambda(wineind)
bc_wineind <- BoxCox(wineind,lambda)

# 3. Take first difference of Box-Cox transformed data.
bc_wineind %>% diff() %>% ggtsdisplay(main="")
bc_d_wineind <- diff(bc_wineind) 

# 4. Take seasonal difference of data.
bc_ds_wineind <- diff(bc_d_wineind, lag=12)
bc_ds_wineind %>% ggtsdisplay(main="")

# 5. Fit an ARIMA model and check residuals.
fit <- Arima(bc_ds_wineind, order=c(0,2,0))
checkresiduals(fit)
