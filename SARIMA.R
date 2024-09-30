library(tseries)
library(forecast)
library(car)

#data is ldeaths
plot(ldeaths)
trans = log(ldeaths) #log data untuk mengurangi variance
plot(trans)

#test to see optimum autoregression and differencing
adf.test(trans)
acf(trans, lag.max=30) #p = semua kecuali (4,10,16,22,28,29)
pacf(trans, lag.max=30) #q = (0,1,2,3,4,10)
#parameter yang tepat p=1, d coba satu satu, q=1 

#Train test split
train = window(trans , start = c(1974,1), end = c(1978,12))
test = window(trans, start = c(1979,1), end = c(1979,12))

### Fitting Model ARIMA w/ train
arima1 = arima(train,order=c(1,1,0),seasonal=list(order=c(1,1,0),period=12))
summary(arima1)
y_train = exp(train)#karena kita log(data), untuk dapet data asli exp(data)
fitted(arima1)
yhat_train = exp(fitted(arima1))#karena kita log(data), untuk dapet data asli exp(data)
error1 = y_train-yhat_train

#uji asumsi
#white noise ljung-box
Box.test(error1,type="Ljung-Box")

#normalitas lillie test or kolmogorov-smirnov
library(nortest)
lillie.test(error1)

#Significance

#Performance measures for test
#MSE
mse_train = mean(error1^2)
mse_train
#RMSE
rmse_train = sqrt(mse_train)
rmse_train
#MAE
mae_train = mean(abs(error1))
mae_train
#MAPE
mape_train = mean(abs(error1/exp(train)))
mape_train

### Predict data and compare to test
yhat_test = predict(arima1, 12, prediction.interval = FALSE)
y_test = exp(test) #karena kita log(data), untuk dapet data asli exp(data)
error2 = y_test-exp(yhat_test$pred) #karena kita log(data), untuk dapet data asli exp(data)
#Performance measures for test
#MSE
mse_test = mean(error2^2)
mse_test
#RMSE
rmse_test = sqrt(mse_test)
rmse_test
#MAE
mae_test = mean(abs(error2))
mae_test
#MAPE
mape_test = mean(abs(error2/y_test))
mape_test

