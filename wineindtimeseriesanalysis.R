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
ggAcf(wineind) +
  ggtitle("Autocorrelation plot (correlogram),\nmonthly Australian wine sales, 1980-1994") +
  theme_light()

## Correlogram, differenced data
diffwineind <- diff(wineind)
ggAcf(diffwineind) +
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
