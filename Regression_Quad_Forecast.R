library(forecast)

## CREATE DATA FRAME. 

# Set working directory for locating files.
setwd("/Users/sarathkumarvatyam/Downloads")

walmart.data <- read.csv("walmart.csv")

#Q1A-
revenue <- walmart.data$Revenue
walmart.ts <- ts(walmart.data$Revenue, 
                 start = c(2005, 1), end = c(2023, 4), freq = 4)

walmart.ts

#Q1B-
# Apply the plot() function to create a plot of the historical data
# Plotting the Time Series Data
plot(walmart.ts, 
     main = "Quarterly Revenue Time Series (2005-2023)",
     xlab = "Year",
     ylab = "Revenue",
     type = "l",  # 'l' for line plot
     col = "blue"
)


#Q2A-
# Data Partition
nValid <- 16
nTrain <- length(walmart.ts) - nValid
train.ts <- window(walmart.ts, start = c(2005, 1), end = c(2019, 4))
valid.ts <- window(walmart.ts, start = c(2020, 1), end = c(2023, 4))
train.ts
valid.ts

## FIT REGRESSION MODEL WITH LINEAR TREND: MODEL 1. 
## FORECAST AND PLOT DATA, AND MEASURE ACURACY.

# Use tslm() function (time series linear model) to create regression model with 
# linear trend.
train.lin <- tslm(train.ts ~ trend)

# See summary of linear trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make forecast for validation period.
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred

# Plot ts data, linear trend, and forecast for the validation period.
plot(train.lin.pred$mean, 
     xlab = "Time", ylab = "Revenue (in 000s)", ylim = c(70000, 180000), 
     bty = "l", xlim = c(2005, 2023), xaxt = "n",
     main = "Regression Model with Linear Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2005, 2023, 1), labels = format(seq(2005, 2023, 1)))
lines(train.lin.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005, 170000, legend = c("Revenue Time Series", "Linear Regression for Training Data",
                                "Linear Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2019, 2019), c(0, 180000))
lines(c(2023, 2023), c(0, 180000))
text(2015, 170000, "Training")
text(2020, 170000, "Validation")
text(2024, 170000, "Future")
arrows(2005, 160000, 2019, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2019, 160000, 2023, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 160000, 2025, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures with rounded
# values to 3 decimals.
round(accuracy(train.lin.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH QUADRATIC (POLYNOMIAL) TREND: MODEL 2. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred


# Plot ts data, regression with quadratic trend, and forecast for the validation period.
plot(train.quad.pred$mean, 
     xlab = "Time", ylab = "Revenue (in 000s)", ylim = c(70000, 180000), 
     bty = "l", xlim = c(2005, 2024), xaxt = "n",
     main = "Regression Model with Quadratic Trend", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2005, 2024, 1), labels = format(seq(2005, 2024, 1)))

lines(train.quad.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005, 170000, legend = c("Revenue Time Series", "Quadratic Trend for Training Data",
                                "Quadratic Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(0, 180000))
lines(c(2023, 2023), c(0, 180000))
text(2017, 170000, "Training")
text(2022, 170000, "Validation")
text(2024, 170000, "Future")
arrows(2005, 160000, 2021, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021, 160000, 2023, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 160000, 2025, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for regression models with linear trend and quadratic (polynomial) trend.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH SEASONALITY: MODEL 3. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# If necessary, run the following code to identify seasons.
train.season$data 

# Plot ts data, regression model with seasonality, and forecast for validation period.
plot(train.season.pred$mean, 
     xlab = "Time", ylab = "Shipments (in 000s)", ylim = c(70000, 180000), 
     bty = "l", xlim = c(2005, 2024), xaxt = "n",
     main = "Regression Model with Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2005, 2024, 1), labels = format(seq(2005, 2024, 1)))
lines(train.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005, 170000, legend = c("Revenue Time Series", "Seasonality Model for Training Data",
                                "Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(0, 180000))
lines(c(2024, 2024), c(0, 180000))
text(2017, 170000, "Training")
text(2022, 170000, "Validation")
text(2025.5, 170000, "Future")
arrows(2005, 160000, 2021, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021, 160000, 2024, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2024, 160000, 2027, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for regression models with (1) linear trend, (2) quadratic (polynomial) trend,
# and (3) seasonality.
round(accuracy(train.lin.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)

## FIT REGRESSION MODEL WITH LINEAR TREND AND SEASONALITY: MODEL 4. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create linear trend and seasonal model.
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

# Plot ts data, linear trend and seasonality data, and predictions for validation period.
plot(train.lin.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in 000s)", ylim = c(70000, 180000), 
     bty = "l", xlim = c(2005, 2024), xaxt = "n",
     main = "Regression Model with Linear Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2005, 2024, 1), labels = format(seq(2005, 2024, 1)))
lines(train.lin.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005, 170000, legend = c("Revenue Time Series", 
                                "Linear Trend and Seasonality Model for Training Data",
                                "Linear Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(0, 180000))
lines(c(2023, 2023), c(0, 180000))
text(2017, 170000, "Training")
text(2022, 170000, "Validation")
text(2024, 170000, "Future")
arrows(2005, 160000, 2021, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021, 160000, 2023, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 160000, 2025, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
# Use accuracy() function to identify common accuracy measures
# for various regression models: (1)linear trend, (2) quadratic  
# (polynomial) trend, (3) seasonality, and (4) linear trend and seasonality.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)

## FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY: MODEL 5. 
## FORECAST AND PLOT DATA, AND MEASURE ACCURACY.

# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

# Plot ts data, quadratic trend and seasonality data, and predictions for validation period.
plot(train.quad.season.pred$mean, 
     xlab = "Time", ylab = "Revenue (in 000s)", ylim = c(70000, 180000), 
     bty = "l", xlim = c(2005, 2024), xaxt = "n",
     main = "Regression Model with Quadratic Trend and Seasonality", 
     lty = 2, lwd = 2, col = "blue")
axis(1, at = seq(2005, 2024, 1), labels = format(seq(2005, 2024, 1)))
lines(train.quad.season.pred$fitted, col = "blue", lwd = 2)
lines(train.ts, col = "black", lty = 1)
lines(valid.ts, col = "black", lty = 1)
legend(2005, 170000, legend = c("Revenue Time Series", 
                                "Quadratic Trend and Seasonality Model for Training Data",
                                "Quadratic Trend and Seasonality Forecast for Validation Data"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

# Plot on the chart vertical lines and horizontal arrows
# describing training, validation, and future prediction intervals.
lines(c(2021, 2021), c(0, 180000))
lines(c(2023, 2023), c(0, 180000))
text(2017, 170000, "Training")
text(2022, 170000, "Validation")
text(2024, 170000, "Future")
arrows(2005, 160000, 2021, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2021, 160000, 2023, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 160000, 2025, 160000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Use accuracy() function to identify common accuracy measures
# for various regression models: (1) linear trend,
# (2) quadratic (polynomial) trend, (3) seasonality, 
# (4) linear trend and seasonality, and (5) quadratic trend 
# and seasonality. 
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)

#Q3A - 
# Develop regression models using tslm() function with entire dataset
# 1. Linear Trend with Seasonality
lin.season <- tslm(walmart.ts ~ trend + season)

# See summary of linear trend and seasonality equation and associated parameters.
summary(lin.season)


lin.season.pred <- forecast(lin.season, h = 8, level = 0)
lin.season.pred

#2.Linear Trend 

# Use tslm() function to create linear trend model.
lin.trend <- tslm(walmart.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# linear trend  data in 8 future periods.
lin.trend.pred <- forecast(lin.trend, h = 8, level = 0)
lin.trend.pred

#3 - QUAD Trend
# Use tslm() function to create quadratic trend model.
quad <- tslm(walmart.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(quad)

# Apply forecast() function to make predictions for ts with 
# quadratic trend data in validation set.  
quad.pred <- forecast(quad, h = 8, level = 0)
quad.pred


#Q3B.

round(accuracy(lin.trend.pred$fitted, walmart.ts),3)
round(accuracy(lin.season.pred$fitted, walmart.ts),3)
round(accuracy(quad.pred$fitted, walmart.ts),3)
round(accuracy((naive(walmart.ts))$fitted, walmart.ts), 3)
round(accuracy((snaive(walmart.ts))$fitted, walmart.ts), 3)

