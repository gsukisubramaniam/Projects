# Forecasting Volume based on the historical data
## Import Dataset

library(readxl)
scanner_data <- read_excel("~/Personal/CTS/scanner_data.xlsx",skip = 1)
head(scanner_data)

# Timeseries Forecasting

library(tseries)

actual<-ts(scanner_data$Volume,frequency = 12,start = c(2011,4))
Decompose_Actual <- decompose(actual)
seasonal <- Decompose_Actual$seasonal
trend <- Decompose_Actual$trend
deseason <- actual-seasonal
detrend <- cbind(t<-c(1:60),deseason) 
fit <- lm(deseason ~ t , data = detrend)
summary(fit)
seasonal

plot(deseason)
plot (actual)
