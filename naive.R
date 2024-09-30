head(dataset)
tail(dataset)
train = dataset[1:60,]
test= dataset[61:nrow(dataset),]

###Train data
p = nrow(train)-12
yhat_train = train$x[1:p]
y_train = train$x[13:60]
error_train = y_train-yhat_train
tail(y_train)
tail(error_train)
percentage_train = error_train/y_train
percentage_train
tail(percentage_train)

#MSE
mse_train = mean(error_train^2)
mse_train
#RMSE
rmse_train = sqrt(mse_train)
rmse_train
#MAE
mae_train = mean(abs(error_train))
mae_train
#MAPE
mape_train = mean(abs(percentage_train))
mape_train


###Test data
p+1
nrow(test)
y_test = train$x[p+1:nrow(test)]
yhat_test = test$x
error_test = y_test-yhat_test
percentage_test = error_test/y_test

#MSE
mse_test = mean(error_test^2)
mse_test
#RMSE
rmse_test = sqrt(mse_test)
rmse_test
#MAE
mae_test = mean(abs(error_test))
mae_test
#MAPE
mape_test = mean(abs(percentage_test))
mape_test
