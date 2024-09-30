head(lung)
tail(lung)

#test
datatest = lung
datatest
model_testing = lm(x~., data=lung)
summary(model_testing)
#test

library(caret)
dummy = dummyVars("~.", data = lung)
dummy
hasil = data.frame(predict(dummy,newdata = lung))
head(hasil)

#Remove timeJan karena jika ada n kategori dibutuhkan n-1 dummy
datareg = hasil[,-c(5)]
head(datareg)

#train test split
train = datareg[1:60,]
test = datareg[61:nrow(datareg),]

###Train data
model1 = lm(x~., data=train)
summary(model1)

#asumption hypotheses testing
library(nortest)
library(lmtest)
bptest(model1)
dwtest(model1)
lillie.test(model1$residuals)
#karena tidak lulus uji asumsi tidak perlu test model

#buat lag-1
a = train[2:nrow(train),]
xt1 = train$x[1:nrow(train)-1]
lag1 = data.frame(a,xt1)    
model2 = lm(x~., data = lag1)
summary(model2)
#tes asumsi model2
bptest(model2)
dwtest(model2)
lillie.test(model2$residuals)

###Test data (Assuming that model is viable)
yhat_test = predict(model1, test)
error_test = test$x-yhat_test

#Metrics for test (assume that model is viable)
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
mape_test = mean(abs(error_test/test$x))
mape_test
