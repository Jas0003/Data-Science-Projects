setwd('/Users/jaskaran/Documents/Statistics/FinalExam')
finaldata=read.csv('DATA_FINAL.csv')

summary(finaldata)

any(is.na(finaldata))

install.packages('caret')
library(caret)
install.packages('corrplot')
library(corrplot)

# calculate correlation matrix
correlationMatrix <- cor(finaldata[,1:21])
# summarize the correlation matrix
print(correlationMatrix)
#plot correlation
corrplot(correlationMatrix, method = 'number')

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.8)
# print indexes of highly correlated attributes
print(highlyCorrelated)

finaldata<-finaldata[,-highlyCorrelated]

smp_size <- floor(0.75 * nrow(finaldata))

## set the seed to make your partition reproducible
set.seed(333)
train_ind <- sample(seq_len(nrow(finaldata)), size = smp_size)

train_finaldata <- finaldata[train_ind, ]
test_finaldata <- finaldata[-train_ind, ]




train_scale <- scale(train_finaldata[, 2:15])
test_scale <- scale(test_finaldata[, 2:15])

train_scale<-as.data.frame(train_scale)
test_scale<-as.data.frame(test_scale)

train_scale<-as.data.frame(cbind(train_scale,train_finaldata$Property1))
test_scale<-as.data.frame(cbind(test_scale,test_finaldata$Property1))

names(train_scale)[ncol(train_scale)] <- "Property1"
names(test_scale)[ncol(test_scale)] <- "Property1"
install.packages("Metrics")
library(Metrics)


#Linear Regression
linearMod <- lm(train_scale$Property1 ~ ., data=train_scale) 



# build linear regression model on full data
print(linearMod)
summary(linearMod)
plot(linearMod)


prop_pred_train <- predict(linearMod, train_scale)

## fit on test

prop_pred <- predict(linearMod, test_scale)

actuals_preds_train <- data.frame(cbind(actuals=train_scale$Property1, predicteds=prop_pred_train))

actuals_preds <- data.frame(cbind(actuals=test_scale$Property1, predicteds=prop_pred))  # make actuals_predicteds dataframe.
#correlation_accuracy <- cor(actuals_preds) 

#min_max_accuracy <- mean(apply(actuals_preds_train, 1, min) / apply(actuals_preds_train, 1, max))  
# => 89.48%, min_max accuracy--------
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 

# => 75.23%, mean absolute percentage deviation
mape_train <- mean(abs((actuals_preds_train$predicteds - actuals_preds_train$actuals))/actuals_preds_train$actuals)  

#########metrics######
rmse=rmse(actuals_preds$actuals,actuals_preds$predicteds)
rmse_train=rmse(actuals_preds_train$actuals,actuals_preds_train$predicteds)
rmse
rmse_train
mae<-mae(actuals_preds$actuals,actuals_preds$predicteds)
mae_train=mae(actuals_preds_train$actuals,actuals_preds_train$predicteds)
mse=mean((actuals_preds$actuals - actuals_preds$predicteds)^2)
mse
mae
########Regression Trees####

library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
#any(is.na(train_df))



fit <- rpart(Property1~.,data = train_finaldata)

rpart.plot(fit)###Image
printcp(fit)

predict_unseen <-predict(fit, test_scale)
predict_unseen<-as.data.frame(predict_unseen)

rmse_cart=rmse(test_scale$Property1,predict_unseen$predict_unseen)
mse_cart=mse(test_scale$Property1,predict_unseen$predict_unseen)
mae_cart=mae(test_scale$Property1,predict_unseen$predict_unseen)




predict_unseen_train <-predict(fit, train_scale)
predict_unseen_train<-as.data.frame(predict_unseen_train)

rmse_cart_train=rmse(train_scale$Property1,predict_unseen_train$predict_unseen_train)




#####knn#####
knnmodel = knnreg(train_scale[,1:14], train_scale[,15])

str(knnmodel)
knnmodel

pred_y = predict(knnmodel, test_scale[,1:14])
pred_y<-as.data.frame(pred_y)
rmse_knn=rmse(test_scale$Property1,pred_y$pred_y)
mse_knn=mse(test_scale$Property1,pred_y$pred_y)
mae_knn=mae(test_scale$Property1,pred_y$pred_y)
rmse_knn
mse_knn
mae_knn
pred_y_train = predict(knnmodel, train_scale[,1:14])
pred_y_train<-as.data.frame(pred_y_train)
rmse_knn_train=rmse(train_scale$Property1,pred_y_train$pred_y_train)
rmse_knn_train
x = 1:length(test_scale$Property1)






##NN

install.packages('neuralnet')



#finaldata=read.csv('DATA_FINAL.csv')

maxs <- apply(finaldata, 2, max) 
mins <- apply(finaldata, 2, min)
scaled <- as.data.frame(scale(finaldata, center = mins, scale = maxs - mins))

set.seed(333)
train_ind <- sample(seq_len(nrow(finaldata)), size = smp_size)


#index <- sample(1:nrow(finaldata),round(0.75*nrow(finaldata)))
train_ <- scaled[train_ind,]
test_ <- scaled[-train_ind,]

library(neuralnet)
n <- names(train_scale)
f <- as.formula(paste("Property1 ~", paste(n[!n %in% "Property1"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,2),linear.output=T)
plot(nn)

pr.nn <- compute(nn,test_)
pr.nn_ <- pr.nn$net.result*(max(finaldata$Property1)-min(finaldata$Property1))+min(finaldata$Property1)
test.r <- (test_$Property1)*(max(finaldata$Property1)-min(finaldata$Property1))+min(finaldata$Property1)

rmse_nn=rmse(test.r,pr.nn_)
mse_nn=mse(test.r,pr.nn_)
mae_nn=mae(test.r,pr.nn_)

rmse_nn
mse_nn
mae_nn
pr.nn_train <- compute(nn,train_)
pr.nn_train <- pr.nn_train$net.result*(max(finaldata$Property1)-min(finaldata$Property1))+min(finaldata$Property1)
train.r <- (train_$Property1)*(max(finaldata$Property1)-min(finaldata$Property1))+min(finaldata$Property1)

rmse_nn_train=rmse(train.r,pr.nn_train)
rmse_nn_train




##Gradient Boosting
install.packages('gbm')
library(gbm)

gbm.fit <- gbm(
  formula = Property1 ~ .,
  distribution = "gaussian",
  data = train_scale,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")


pred <- predict(gbm.fit, n.trees = gbm.fit$n.trees, test_scale)


rmse_gbm=rmse(test_scale$Property1,pred)
mse_gbm=mse(test_scale$Property1,pred)
mae_gbm=mae(test_scale$Property1,pred)
rmse_gbm
mse_gbm
mae_gbm
pred_train <- predict(gbm.fit, n.trees = gbm.fit$n.trees, train_scale)


rmse_gbm_train=rmse(train_scale$Property1,pred_train)
rmse_gbm_train
#cor(test_finaldata$Property1, pred) ^ 2




#chk<-cbind(as.data.frame(pred),test_finaldata$Property1)

# results

#####Random Forest
library(randomForest)
rf_model <- randomForest(Property1 ~ . ,
                         data = train_scale,
                         
                         importance=TRUE)

varImp(rf_model)



y_hat <- predict(rf_model, test_scale)
rmse_rf=rmse(test_scale$Property1,y_hat)
mse_rf=mse(test_scale$Property1,y_hat)
mae_rf=mae(test_scale$Property1,y_hat)
rmse_rf
mse_rf
mae_rf
y_hat_train <- predict(rf_model, train_scale)
rmse_rf_train=rmse(train_scale$Property1,y_hat_train)
rmse_rf_train

##Bagging

install.packages('ipred')
library(ipred)
library(caret)
library(Metrics)
fit_bagging <- bagging(Property1~., data = train_scale, coob = T, nbagg = 100)

print(fit_bagging)
y_hat_bag <- predict(fit_bagging, test_scale)
rmse_rf_bag=rmse(test_scale$Property1,y_hat_bag)
mse_rf_bag=mse(test_scale$Property1,y_hat_bag)
mae_rf_bag=mae(test_scale$Property1,y_hat_bag)
rmse_rf_bag
mse_rf_bag
mae_rf_bag
y_hat_bag_train <- predict(fit_bagging, train_scale)
rmse_rf_bag_train=rmse(train_scale$Property1,y_hat_bag_train)
rmse_rf_bag_train
##rfe

library(randomForest)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(train_finaldata[,2:15], train_finaldata[,1], sizes=c(1:50), rfeControl=control,
               allowParallel=TRUE)
# summarize the results
print(results)
# list the chosen features
predictors(results)
