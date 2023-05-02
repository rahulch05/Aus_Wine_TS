## USE FORECAST LIBRARY.

library(forecast)
library(zoo)
# library(ggplot2)

# Set working directory for locating files.
setwd("/Users/rahulchada/Documents/673 Times series analytics")

# Create data frame.
Auswinesales.data <- read.csv("AusWineSales.csv")

# See the first 6 records of the file.
head(Auswinesales.data)

# See the last 6 records of the file.
tail(Auswinesales.data)

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12.
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
redwine.ts <- ts(Auswinesales.data$Red,
                   start = c(1980, 1), end = c(1994, 12), freq = 12)

sparkling.ts <- ts(Auswinesales.data$Sparkling,
                 start = c(1980, 1), end = c(1994, 12), freq = 12)

sweetwhite.ts <- ts(Auswinesales.data$Sweetwhite,
                 start = c(1980, 1), end = c(1994, 12), freq = 12)


# Use stl() function to plot times series components of the original data.
# The plot includes original data, trend, seasonal, and reminder
# (level and noise component).
redwinesales.stl <- stl(redwine.ts, s.window = "periodic")
autoplot(redwinesales.stl, main = "Red Wine Sales in Aus. Time Series Components")

sparkwinesales.stl <- stl(sparkling.ts, s.window = "periodic")
autoplot(sparkwinesales.stl, main = "Sparkling Wine Sales in Aus. Time Series Components")

sweetwinesales.stl <- stl(sweetwhite.ts, s.window = "periodic")
autoplot(sweetwinesales.stl, main = "Sweet White Wine Sales in Aus. Time Series Components")

## Use plot() to plot time series data  Red Wine
red_plt <- plot(redwine.ts,
     xlab = "Time", ylab = "Sales",
     ylim = c(500, 4000), xaxt = 'n',
     main = "Red wine sales in Australia")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))

## Use plot() to plot time series data of Sparkling wine
spark_plt <- plot(sparkling.ts,
     xlab = "Time", ylab = "Sales",
     ylim = c(900, 7000), xaxt = 'n',
     main = "Sparkling wine sales in Australia")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))

## Use plot() to plot time series data Sweetwhite wine
sweet_plt <- plot(sweetwhite.ts,
     xlab = "Time", ylab = "Sales",
     ylim = c(100, 700), xaxt = 'n',
     main = "Sweetwhite wine sales in Australia")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(1980, 1994, 1), labels = format(seq(1980, 1994, 1)))

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(redwine.ts, lag.max = 12,
               main = "Autocorrelation for Red Wine sales in Australia")

autocor <- Acf(sparkling.ts, lag.max = 12,
               main = "Autocorrelation for Sparkling wine sales in Australia")

autocor <- Acf(sweetwhite.ts, lag.max = 12,
               main = "Autocorrelation for Sweetwhite wine sales in Australia")


# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
# Total number of period length(prod.ts) = 252.
# nvalid = 36 months (3 years), from January 1992 to December 1994.
# nTrain = 144  months (12 years), from January 1980 to December 1991.
nValid <- 36
nTrain <- length(redwine.ts) - nValid
red_train.ts <- window(redwine.ts, start = c(1980, 1), end = c(1980, nTrain))
red_valid.ts <- window(redwine.ts, start = c(1980, nTrain + 1),
                   end = c(1980, nTrain + nValid))


nTrain <- length(sparkling.ts) - nValid
spark_train.ts <- window(sparkling.ts, start = c(1980, 1), end = c(1980, nTrain))
spark_valid.ts <- window(sparkling.ts, start = c(1980, nTrain + 1),
                   end = c(1980, nTrain + nValid))


nTrain <- length(sweetwhite.ts) - nValid
white_train.ts <- window(sweetwhite.ts, start = c(1980, 1), end = c(1980, nTrain))
white_valid.ts <- window(sweetwhite.ts, start = c(1980, nTrain + 1),
                   end = c(1980, nTrain + nValid))

# Use Arima() function to fit AR(1) model Red wine data.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
redwine.ar1<- Arima(redwine.ts, order = c(1,0,0))
summary(redwine.ar1)

# Apply z-test to test the null hypothesis that beta
# coefficient of AR(1) is equal to 1.
ar1 <- 0.7418
s.e. <- 0.0511
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


# Use Arima() function to fit AR(1) model Shipments data.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
sparkling.ar1<- Arima(sparkling.ts, order = c(1,0,0))
summary(sparkling.ar1)

# Apply z-test to test the null hypothesis that beta
# coefficient of AR(1) is equal to 1.
ar1 <- 0.3918
s.e. <- 0.0700
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}


# Use Arima() function to fit AR(1) model Shipments data.
# The ARIMA model of order = c(1,0,0) gives an AR(1) model.
sweetwhite.ar1<- Arima(sweetwhite.ts, order = c(1,0,0))
summary(sweetwhite.ar1)

# Apply z-test to test the null hypothesis that beta
# coefficient of AR(1) is equal to 1.
ar1 <- 0.7886
s.e. <- 0.0459
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}



# RED WINE (Training Data)
# Two-Level Model with Linear Trend & Seasonality Regression and
# Ar(1) Model for Regression Residuals,
red_train.lin.season <- tslm(red_train.ts ~ trend + season)
summary(red_train.lin.season)

#forecast
red_train.lin.season.pred <- forecast(red_train.lin.season, h = nValid, level = 0)
red_train.lin.season.pred

Acf(red_train.lin.season.pred$residuals, lag.max = 12,
    main = "Autocorrelation for Red Wine Training Residuals")

red_res.ar1 <- Arima(red_train.lin.season$residuals, order = c(1,0,0))
summary(red_res.ar1)

#forecast
red_res.ar1.pred <- forecast(red_res.ar1, h = nValid, level = 0)
red_res.ar1.pred

Acf(red_res.ar1$residuals, lag.max = 12,
    main = "Autocorrelation for Red Wine Training Residuals of Residuals")

# Create data table with validation data, regression forecast
# for validation period, AR(1) residuals for validation, and
# two level model results.
red_train.two.level.pred <- red_train.lin.season.pred$mean + red_res.ar1.pred$mean
red_train.two.level.fitted <- red_train.lin.season.pred$fitted + red_res.ar1.pred$fitted
red_valid.df <- round(data.frame(red_valid.ts, red_train.lin.season.pred$mean,
                             red_res.ar1.pred$mean, red_train.two.level.pred),3)
names(red_valid.df) <- c("Validation Data", "Reg Forecast",
                     "AR(1) Forecast", "Combined Forecast")
red_valid.df

round(accuracy(red_train.two.level.pred, red_valid.ts), 3)
# Seasonal naive forecast
round(accuracy((snaive(redwine.ts))$fitted, redwine.ts), 3)
# Naive forecast
round(accuracy((naive(redwine.ts))$fitted, redwine.ts), 3)


# Plotting the two level model
plot(red_train.two.level.pred,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(500, 3900),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Red Wine Two Level Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(red_train.two.level.fitted, col = "blue", lwd = 2)
lines(redwine.ts)
legend(1980,3900,
       legend = c("Sales",
                  "Two Level Model for Training Partition",
                  "Two Level Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 3900))
lines(c(1995, 1995), c(0, 3900))
text(1987, 3900, "Training")
text(1993.5, 3900, "Validation")
text(1996.5, 3900, "Future")
arrows(1980, 3700, 1991.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 3700, 1994.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 3700, 1997.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Holtz-Winters Model red wine training data.
red_hw.ZZZ <- ets(red_train.ts, model = "ZZZ")
red_hw.ZZZ

red_hw.ZZZ.pred <- forecast(red_hw.ZZZ, h = nValid, level = 0)
red_hw.ZZZ.pred

round(accuracy(red_hw.ZZZ.pred$mean, red_valid.ts), 3)

# plotting HW model for red wine
plot(red_hw.ZZZ.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(500, 3900),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Red Wine Holt-Winter's Model with Optimal Smoothing Parameters",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1995, 1), labels = format(seq(1980, 1995, 1)))
lines(red_hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(redwine.ts)
legend(1980,3900,
       legend = c("Sales",
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 3900))
lines(c(1995, 1995), c(0, 3900))
text(1987, 3900, "Training")
text(1993.5, 3900, "Validation")
text(1996.5, 3900, "Future")
arrows(1980, 3700, 1991.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 3700, 1994.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 3700, 1997.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# auto ARIMA red wine training data.
red.auto.arima <- auto.arima(red_train.ts)
summary(red.auto.arima)

red.auto.arima.pred <- forecast(red.auto.arima, h = nValid, level = 0)
red.auto.arima.pred

round(accuracy(red.auto.arima.pred$mean, red_valid.ts), 3)

# Plotting the auto arima model for red model
plot(red.auto.arima.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(500, 3900),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Red Wine Auto ARIMA Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(red.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(redwine.ts)
legend(1980,3900,
       legend = c("Sales",
                  "Auto ARIMA Model for Training Partition",
                  "Auto ARIMA Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 3900))
lines(c(1995, 1995), c(0, 3900))
text(1987, 3900, "Training")
text(1993.5, 3900, "Validation")
text(1996.5, 3900, "Future")
arrows(1980, 3700, 1991.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 3700, 1994.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 3700, 1997.5, 3700, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# SPARKLING WINE (Training Data)
# Two-Level Model with Linear Trend & Seasonality Regression and
# Ar(1) Model for Regression Residuals,
sparkling_train.lin.season <- tslm(spark_train.ts ~ trend + season)
summary(sparkling_train.lin.season)

#forecast
sparkling.lin.season.pred <- forecast(sparkling_train.lin.season, h = nValid, level = 0)
sparkling.lin.season.pred

Acf(sparkling.lin.season.pred$residuals, lag.max = 12,
    main = "Autocorrelation for sparkling Wine Training Residuals")

sparkling_res.ar1 <- Arima(sparkling_train.lin.season$residuals, order = c(1,0,0))
summary(sparkling_res.ar1)

#forecast
sparkling_res.ar1.pred <- forecast(sparkling_res.ar1, h = nValid, level = 0)
sparkling_res.ar1.pred

Acf(sparkling_res.ar1$residuals, lag.max = 12,
    main = "Autocorrelation for sparkling Wine Training Residuals of Residuals")

sparkling_train.two.level.pred <- sparkling.lin.season.pred$mean + sparkling_res.ar1.pred$mean
sparkling_train.two.level.fitted <- sparkling.lin.season.pred$fitted + sparkling_res.ar1.pred$fitted

sparkling_valid.df <- round(data.frame(spark_valid.ts, sparkling.lin.season.pred$mean,
                                       sparkling_res.ar1.pred$mean, sparkling_train.two.level.pred),3)
names(sparkling_valid.df) <- c("Validation Data", "Reg Forecast",
                         "AR(1) Forecast", "Combined Forecast")
sparkling_valid.df

round(accuracy(sparkling_train.two.level.pred, spark_valid.ts), 3)
# Seasonal naive forecast
round(accuracy((snaive(sparkling.ts))$fitted, sparkling.ts), 3)
# Naive forecast
round(accuracy((naive(sparkling.ts))$fitted, sparkling.ts), 3)


# Plotting the two level model for sparkling wine
plot(sparkling_train.two.level.pred,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(1000, 8000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sparkling Wine Two Level Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(sparkling_train.two.level.fitted, col = "blue", lwd = 2)
lines(sparkling.ts)
legend(1980,8000,
       legend = c("Sales",
                  "Two Level Model for Training Partition",
                  "Two Level Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 8000))
lines(c(1995, 1995), c(0, 8000))
text(1987, 8000, "Training")
text(1993.5, 8000, "Validation")
text(1996.5, 8000, "Future")
arrows(1980, 7750, 1991.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 7750, 1994.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 7750, 1997.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Holtz-Winters Model sparkling wine training data.
sparkling_hw.ZZZ <- ets(spark_train.ts, model = "ZZZ")
sparkling_hw.ZZZ

sparkling_hw.ZZZ.pred <- forecast(sparkling_hw.ZZZ, h = nValid, level = 0)
sparkling_hw.ZZZ.pred

round(accuracy(sparkling_hw.ZZZ.pred$mean, spark_valid.ts), 3)


# Plotting the Holts-Winters model for sparkling wine
plot(sparkling_hw.ZZZ.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(1000, 8000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sparkling Wine Holt-Winter's Model with Optimal Smoothing Parameters",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(sparkling_hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sparkling.ts)
legend(1980,8000,
       legend = c("Sales",
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 8000))
lines(c(1995, 1995), c(0, 8000))
text(1987, 8000, "Training")
text(1993.5, 8000, "Validation")
text(1996.5, 8000, "Future")
arrows(1980, 7750, 1991.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 7750, 1994.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 7750, 1997.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# auto ARIMA sparkling wine training data.
sparkling.auto.arima <- auto.arima(spark_train.ts)
summary(sparkling.auto.arima)

sparkling.auto.arima.pred <- forecast(sparkling.auto.arima, h = nValid, level = 0)
sparkling.auto.arima.pred

round(accuracy(sparkling.auto.arima.pred$mean, spark_valid.ts), 3)

# Plotting the auto Arima model with sparkling wine
plot(sparkling.auto.arima.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(1000, 8000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sparkling Wine Auto ARIMA Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(sparkling.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(sparkling.ts)
legend(1980,8000,
       legend = c("Sales",
                  "Auto ARIMA Model for Training Partition",
                  "Auto ARIMA Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 8000))
lines(c(1995, 1995), c(0, 8000))
text(1987, 8000, "Training")
text(1993.5, 8000, "Validation")
text(1996.5, 8000, "Future")
arrows(1980, 7750, 1991.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 7750, 1994.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 7750, 1997.5, 7750, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# SWEETWHITE WINE (Training Data)
# Two-Level Model with Linear Trend & Seasonality Regression and
# Ar(1) Model for Regression Residuals,
sweetwhite_train.lin.season <- tslm(white_train.ts ~ trend + season)
summary(sweetwhite_train.lin.season)

#forecast
sweetwhite.lin.season.pred <- forecast(sweetwhite_train.lin.season, h = nValid, level = 0)
sweetwhite.lin.season.pred

Acf(sweetwhite.lin.season.pred$residuals, lag.max = 12,
    main = "Autocorrelation for sweetwhite Wine Training Residuals")

sweetwhite_res.ar1 <- Arima(sweetwhite_train.lin.season$residuals, order = c(1,0,0))
summary(sweetwhite_res.ar1)

#forecast
sweetwhite_res.ar1.pred <- forecast(sweetwhite_res.ar1, h = nValid, level = 0)
sweetwhite_res.ar1.pred

Acf(sweetwhite_res.ar1$residuals, lag.max = 12,
    main = "Autocorrelation for Sweet white Wine Training Residuals of Residuals")

sweetwhite_train.two.level.pred <- sweetwhite.lin.season.pred$mean + sweetwhite_res.ar1.pred$mean
sweetwhite_train.two.level.fitted <- sweetwhite.lin.season.pred$fitted + sweetwhite_res.ar1.pred$fitted

sweetwhite_valid.df <- round(data.frame(white_valid.ts, sweetwhite.lin.season.pred$mean,
                                        sweetwhite_res.ar1.pred$mean, sweetwhite_train.two.level.pred),3)
names(sweetwhite_valid.df) <- c("Validation Data", "Reg Forecast",
                               "AR(1) Forecast", "Combined Forecast")
sweetwhite_valid.df

round(accuracy(sweetwhite_train.two.level.pred, white_valid.ts), 3)
# Seasonal naive forecast
round(accuracy((snaive(sweetwhite.ts))$fitted, sweetwhite.ts), 3)
# Naive forecast
round(accuracy((naive(sweetwhite.ts))$fitted, sweetwhite.ts), 3)

# plot two level model for sweet white wine
plot(sweetwhite_train.two.level.pred,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(50, 1000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sweet White Wine Two Level Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(sweetwhite_train.two.level.fitted, col = "blue", lwd = 2)
lines(sweetwhite.ts)
legend(1980,950,
       legend = c("Sales",
                  "Two Level Model for Training Partition",
                  "Two Level Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 1000))
lines(c(1995, 1995), c(0, 1000))
text(1987, 1000, "Training")
text(1993.5, 1000, "Validation")
text(1996.5, 1000, "Future")
arrows(1980, 950, 1991.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 950, 1994.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 950, 1997.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# Holtz-Winters Model white wine training data.
sweetwhite_hw.ZZZ <- ets(white_train.ts, model = "ZZZ")
sweetwhite_hw.ZZZ

sweetwhite_hw.ZZZ.pred <- forecast(sweetwhite_hw.ZZZ, h = nValid, level = 0)
sweetwhite_hw.ZZZ.pred

round(accuracy(sweetwhite_hw.ZZZ.pred$mean, white_valid.ts), 3)



# Plotting tht sweet white wine HW model
plot(sweetwhite_hw.ZZZ.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(50, 1000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sweet White Wine Holt-Winter's Model with Optimal Smoothing Parameters",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(sweetwhite_hw.ZZZ.pred$fitted, col = "blue", lwd = 2)
lines(sweetwhite.ts)
legend(1980,950,
       legend = c("Sales",
                  "Holt-Winter's Model for Training Partition",
                  "Holt-Winter's Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 1000))
lines(c(1995, 1995), c(0, 1000))
text(1987, 1000, "Training")
text(1993.5, 1000, "Validation")
text(1996.5, 1000, "Future")
arrows(1980, 950, 1991.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 950, 1994.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 950, 1997.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)




# auto ARIMA white wine training data.
white.auto.arima <- auto.arima(white_train.ts)
summary(auto.arima)

white.auto.arima.pred <- forecast(white.auto.arima, h = nValid, level = 0)
white.auto.arima.pred

round(accuracy(white.auto.arima.pred$mean, white_valid.ts), 3)

# plotting the white wine auto arima model
plot(white.auto.arima.pred$mean,
     xlab = "Time", ylab = "Sales (in 000s of Liters)", ylim = c(50, 1000),
     bty = "l", xlim = c(1980, 1997), xaxt = "n",
     main = "Sweet White Wine Auto ARIMA Model",
     lty = 2, col = "blue", lwd = 2)
axis(1, at = seq(1980, 1997, 1), labels = format(seq(1980, 1997, 1)))
lines(white.auto.arima.pred$fitted, col = "blue", lwd = 2)
lines(sweetwhite.ts)
legend(1980,950,
       legend = c("Sales",
                  "Auto ARIMA Model for Training Partition",
                  "Auto ARIMA Model for Validation Partition"),
       col = c("black", "blue" , "blue"),
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(1992, 1992), c(0, 1000))
lines(c(1995, 1995), c(0, 1000))
text(1987, 1000, "Training")
text(1993.5, 1000, "Validation")
text(1996.5, 1000, "Future")
arrows(1980, 950, 1991.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1992.5, 950, 1994.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1995.5, 950, 1997.5, 950, code = 3, length = 0.1,
       lwd = 1, angle = 30)



# ENTIRE DATA SET


##RED WINE##


## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET. 
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
red.tot.trend.seas <- tslm(redwine.ts ~ trend  + season)
summary(red.tot.trend.seas)

# Create regression forecast for future 12 periods.
red.tot.trend.seas.pred <- forecast(red.tot.trend.seas, h = 12, level = 0)
red.tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
red.tot.trend.seas.res <- red.tot.trend.seas$residuals
red.tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
red.tot.ma.trail.res <- rollmean(red.tot.trend.seas.res, k = 4, align = "right")
red.tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
red.tot.ma.trail.res.pred <- forecast(red.tot.ma.trail.res, h = 12, level = 0)
red.tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
red.tot.fst.2level <- red.tot.trend.seas.pred$mean + red.tot.ma.trail.res.pred$mean
red.tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
red.future12.df <- round(data.frame(red.tot.trend.seas.pred$mean, red.tot.ma.trail.res.pred$mean, 
                                    red.tot.fst.2level), 3)
names(red.future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
red.future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(red.tot.trend.seas.pred$fitted, redwine.ts), 3)
round(accuracy(red.tot.trend.seas.pred$fitted+red.tot.ma.trail.res, redwine.ts), 3)
round(accuracy((snaive(redwine.ts))$fitted, redwine.ts), 3)


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full production data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
RED.HW.ZZZ <- ets(redwine.ts, model = "ZZZ")
RED.HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
RED.HW.ZZZ.pred <- forecast(RED.HW.ZZZ, h = 12 , level = 0)
RED.HW.ZZZ.pred

## FORECAST WITH ARIMA MODELS FOR ENTIRE DATA SET. 
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
red.auto.arima <- auto.arima(redwine.ts)
summary(red.auto.arima)

# Apply forecast() function to make predictions for ts with 
# auto ARIMA model for the future 8 periods. 
red.auto.arima.pred <- forecast(red.auto.arima, h = 12, level = 0)
red.auto.arima.pred

# Identify performance measures for HW forecast.
# Use accuracy() function to identify common accuracy measures.
round(accuracy((snaive(redwine.ts))$fitted, redwine.ts), 3)
round(accuracy(RED.HW.ZZZ.pred$fitted, redwine.ts), 3)
round(accuracy(red.auto.arima.pred$fitted, redwine.ts), 3)


##SPARKLING WINE##

## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET.
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
sparkling.tot.trend.seas <- tslm(sparkling.ts ~ trend  + season)
summary(sparkling.tot.trend.seas)

# Create regression forecast for future 12 periods.
sparkling.tot.trend.seas.pred <- forecast(sparkling.tot.trend.seas, h = 12, level = 0)
sparkling.tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
sparkling.tot.trend.seas.res <- sparkling.tot.trend.seas$residuals
sparkling.tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
sparkling.tot.ma.trail.res <- rollmean(sparkling.tot.trend.seas.res, k = 4, align = "right")
sparkling.tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
sparkling.tot.ma.trail.res.pred <- forecast(sparkling.tot.ma.trail.res, h = 12, level = 0)
sparkling.tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining
# regression forecast and trailing MA for residuals for future
# 12 periods.
sparkling.tot.fst.2level <- sparkling.tot.trend.seas.pred$mean + sparkling.tot.ma.trail.res.pred$mean
sparkling.tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
sparkling.future12.df <- round(data.frame(sparkling.tot.trend.seas.pred$mean, sparkling.tot.ma.trail.res.pred$mean,
                                          sparkling.tot.fst.2level), 3)
names(sparkling.future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
sparkling.future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(sparkling.tot.trend.seas.pred$fitted, sparkling.ts), 3)
round(accuracy(sparkling.tot.trend.seas.pred$fitted+sparkling.tot.ma.trail.res, sparkling.ts), 3)
round(accuracy((snaive(sparkling.ts))$fitted, sparkling.ts), 3)


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full production data set.
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
SPARK.HW.ZZZ <- ets(sparkling.ts, model = "ZZZ")
SPARK.HW.ZZZ

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
SPARK.HW.ZZZ.pred <- forecast(SPARK.HW.ZZZ, h = 12 , level = 0)
SPARK.HW.ZZZ.pred

## FORECAST WITH ARIMA MODELS FOR ENTIRE DATA SET.
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
spark.auto.arima <- auto.arima(sparkling.ts)
summary(spark.auto.arima)

# Apply forecast() function to make predictions for ts with
# auto ARIMA model for the future 8 periods.
spark.auto.arima.pred <- forecast(spark.auto.arima, h = 12, level = 0)
spark.auto.arima.pred

# Identify performance measures for HW forecast.
# Use accuracy() function to identify common accuracy measures.
round(accuracy((snaive(sparkling.ts))$fitted, sparkling.ts), 3)
round(accuracy(SPARK.HW.ZZZ.pred$fitted, sparkling.ts), 3)
round(accuracy(spark.auto.arima.pred$fitted, sparkling.ts), 3)


##SWEET WHITE##


## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET.
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
sweetwhite.tot.trend.seas <- tslm(sweetwhite.ts ~ trend  + season)
summary(sweetwhite.tot.trend.seas)

# Create regression forecast for future 12 periods.
sweetwhite.tot.trend.seas.pred <- forecast(sweetwhite.tot.trend.seas, h = 12, level = 0)
sweetwhite.tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
sweetwhite.tot.trend.seas.res <- sweetwhite.tot.trend.seas$residuals
sweetwhite.tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
sweetwhite.tot.ma.trail.res <- rollmean(sweetwhite.tot.trend.seas.res, k = 4, align = "right")
sweetwhite.tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
sweetwhite.tot.ma.trail.res.pred <- forecast(sweetwhite.tot.ma.trail.res, h = 12, level = 0)
sweetwhite.tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining
# regression forecast and trailing MA for residuals for future
# 12 periods.
sweetwhite.tot.fst.2level <- sweetwhite.tot.trend.seas.pred$mean + sweetwhite.tot.ma.trail.res.pred$mean
sweetwhite.tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
sweetwhite.future12.df <- round(data.frame(sweetwhite.tot.trend.seas.pred$mean, sweetwhite.tot.ma.trail.res.pred$mean,
                                           sweetwhite.tot.fst.2level), 3)
names(sweetwhite.future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
sweetwhite.future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(sweetwhite.tot.trend.seas.pred$fitted, sweetwhite.ts), 3)
round(accuracy(sweetwhite.tot.trend.seas.pred$fitted+sweetwhite.tot.ma.trail.res, sweetwhite.ts), 3)
round(accuracy((snaive(sweetwhite.ts))$fitted, sweetwhite.ts), 3)


## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full production data set.
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
WHITE.HW.ZZZ <- ets(sweetwhite.ts, model = "ZZZ")
WHITE.HW.ZZZ

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
WHITE.HW.ZZZ.pred <- forecast(WHITE.HW.ZZZ, h = 12 , level = 0)
WHITE.HW.ZZZ.pred

## FORECAST WITHARIMA MODELS FOR ENTIRE DATA SET.
# Use auto.arima() function to fit ARIMA model for entire data set.
# use summary() to show auto ARIMA model and its parameters for entire data set.
white.auto.arima <- auto.arima(sweetwhite.ts)
summary(white.auto.arima)

# Apply forecast() function to make predictions for ts with
# auto ARIMA model for the future 8 periods.
white.spark.auto.arima.pred <- forecast(white.auto.arima, h = 12, level = 0)
white.spark.auto.arima.pred

# Identify performance measures for HW forecast.
# Use accuracy() function to identify common accuracy measures.
round(accuracy((snaive(sweetwhite.ts))$fitted, sweetwhite.ts), 3)
round(accuracy(WHITE.HW.ZZZ.pred$fitted, sweetwhite.ts), 3)
round(accuracy(white.auto.arima.pred$fitted, sweetwhite.ts), 3)

