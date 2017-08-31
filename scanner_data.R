## Import Dataset
# Renamed the column names in Excel removing the special characters and spaces

library(readxl)
scanner_data <- read_excel("~/Personal/CTS/scanner_data.xlsx",skip = 1)
head(scanner_data)

## Getting the columns as individual variables to test the assumptions

vol <- scanner_data$Volume
price <- scanner_data$Price
packsize <- scanner_data$Pack_Size
distribution <- scanner_data$Distribution
merch <- scanner_data$Merch

## Assumptions & Validating the Assumptions
## All the independent variables have correlation with Dependent variable
## Testing the linear relashionship of x (Independent variables) with y (Dependent variable)

cor(vol,price)
plot(price, vol)

cor(packsize, vol)
plot (packsize, vol)

cor (distribution, vol)
plot (distribution, vol)

cor (merch, vol)
plot (merch, vol)

cor (vol,scanner_data$dummy)
plot (vol,scanner_data$dummy)

## Validating Normal distribution in Independent Variables

shapiro.test(scanner_data$Price)
shapiro.test((scanner_data$Distribution))
shapiro.test(scanner_data$Pack_Size)
shapiro.test(scanner_data$Merch)
shapiro.test((scanner_data$dummy))


## Correlation Matrix

cor_matrix <- cor(scanner_data[,2:7])
library(corrplot)
corrplot(cor_matrix,method = "circle")


## Creating Training & Test Data sets

#Sample Indexes

indexes = sample(1:nrow(scanner_data), size=0.3*nrow(scanner_data))

# Split data between Training and Test data sets (70% to Training; 30% to Test)

set.seed(123)
test = scanner_data[indexes,]
dim(test)
set.seed(123)
train = scanner_data[-indexes,]
dim(train) 

## Model

## Model with 1 Independent variable (Price)

fit1 <- lm (data = train , (Volume) ~ (Price))
summary(fit1)

## Model with 2 Independent variable (Price & Packsize)
fit2 <- lm (data = train , (Volume) ~ (Price)+(Pack_Size))
summary(fit2)

## Model with 3 Independent variable (Price, Packsize & Distribution)
fit3 <- lm (data =train , (Volume) ~ (Price)+(Pack_Size) + (Distribution))
summary(fit3)

## Model with 4 Independent variable (Price, Packsize, Distribution & Merch %)
fit4 <- lm (data = train, (Volume) ~ (Price)+(Pack_Size) + (Distribution)
            + (Merch))
summary(fit4) 
# Significance of Price is affected in the above model with the existence of "Merch %" which has a high negative correlation with Price.


## Model with 5 Independent variable (Price, Packsize, Distribution, Merch % & Dummy variable)
fit5 <- lm (data = train , (Volume) ~ (Price)+(Pack_Size) + (Distribution)
            + (Merch)
            + (dummy))
summary(fit5)

## Iterations to the Model 
# Excluding Pack Size and Merch% from the model. 
# Strong negative correlation (-0.71) exists between Price and Merch% and hence one of the variable not to be considered in the model to avoid multi collinearity.
# Assuming "Price" variable is more important in business perspective compared to "Merch %", removing Merch% from the model.
# Pack size do not have significant correlation with Volume and hence removed from the model.

fit <- lm( data = train, (Volume)~
             (Price)+
             (Distribution)+
             #(Pack_Size)+
             #(Merch)+
             (dummy))
summary(fit)

## To find scaled beta

scale_fit <- lm (scale(Volume) ~ scale(Price)+ scale(Distribution) + dummy, data = train)
summary(scale_fit)

# Mean square Error 

mad <- mean(abs(fit$residuals))
mad
mse <- mean(fit$residuals^2)
mse
rmse <- sqrt(mse)
rmse
mape <- mean(abs(fit$residuals)/train$Volume)
mape*100 

# AIC & BIC

AIC(scale_fit)
BIC(scale_fit)

# predicting volume in test data

pred <- predict(fit,newdata = test)
pred
res_pred <- test$Volume-pred

res## Testing if there is Multicollinearity between the Independent Variables
library(car)
vif(fit)



# Analyzing the Residuals in Test data
# To check if the Residuals are normally distributed

fit$residuals
hist(fit$residuals, col = "grey")
shapiro.test(fit$residuals)
shapiro.test(res_pred)
# p-value is > 0.05 and hence residuals have normal distribution

# ***


# plot Actual volume vs. Residual

plot(train$Volume,fit$residuals)
plot(test$Volume, res_pred)
plot(test$Price, res_pred)
plot(test$Distribution, res_pred)


# Forecasting Volume using Dynamic Regression

install.packages("forecast")
library(forecast)
library(arima)
volumets <- ts(scanner_data$Volume)
plot(volumets)
x <- scanner_data[,c(3,5)]

forecast_vol <- auto.arima(volumets, allowdrift = TRUE, xreg = x) 
forecast_vol
xnew <- x[49:60,]
xnew
forecast_volume <- forecast.Arima(forecast_vol, h=3, xreg = xnew)  
forecast_volume
plot(forecast_volume)
