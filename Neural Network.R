library(tseries)
library(forecast)
library(car)
library(datasets)
plot(ldeaths)

# Train test split
train = ldeaths[1:60]
test = ldeaths[61:length(ldeaths)]

#Fitting with train
nn1 = nnetar(train, P=2, size=3)
plot(forecast(nn1))
nn1$model
nn1$fitted
accuracy(nn1)

#Testing
yhat_test = forecast(nn1, h=length(test))
yhat_test
