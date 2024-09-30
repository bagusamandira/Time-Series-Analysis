#prepare data
dataset$x
plot(ldeaths)
datats = ts(dataset$x, start = c(1974,1), end = c(1979,12), frequency = 12)

#model1
tes1 = HoltWinters(datats,alpha=0.5,beta=0.1,
                   gamma=0.8,
                   seasonal="multiplicative")
summary(tes1)
plot(tes1)
err1 = ldeaths[13:72]-tes1$fitted[,1]
#MSE
mse1 = mean(err1^2)
mse1
#RMSE
rmse1 = sqrt(mse1)
rmse1
#MAE
mae1 = mean(abs(err1))
mae1
#MAPE
mape1 = mean(abs(err1/ldeaths[13:72]))
mape1

#model2
tes2 = HoltWinters(ldeaths)
tes2
summary(tes2)
err2 = ldeaths[13:72]-tes2$fitted[,1]
plot(tes2)
#MSE
mse2 = mean(abs(err2^2))
mse2
#RMSE
rmse1 = sqrt(mse2)
rmse1
#MAE
mae2 = mean(abs(err2))
mae2
#MAPE
mape2 = mean(abs(err2/ldeaths[13:72]))
mape2
