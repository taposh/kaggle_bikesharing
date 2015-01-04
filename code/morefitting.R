setwd('~/workspace/kaggle_bikesharing/Tutorial')
require(doMC)
registerDoMC(cores=4)

library(Metrics)

require(lubridate)
require(caret)
# preprocessing
train <- read.csv('../DATA/train.csv', head=T, stringsAsFactor=F)
test <- read.csv('../DATA/test.csv', head=T, stringsAsFactor=F)
train$hour <- hour(train$datetime)
test$hour <- hour(test$datetime)
train$wd <- wday(train$datetime)
test$wd <- wday(test$datetime)
test$count <- 0
head(test)
head(train)
train <- train[,-1]
train <- train[,-10]
train <- train[,-9]
test <- test[,-1]
train$season <- as.factor(train$season)
train$holiday <- as.factor(train$holiday)
train$workingday <- as.factor(train$workingday)
train$weather <- as.factor(train$weather)
train$hour <- as.factor(train$hour)
train$wd <- as.factor(train$wd)
test$season <- as.factor(test$season)
test$holiday <- as.factor(test$holiday)
test$workingday <- as.factor(test$workingday)
test$weather <- as.factor(test$weather)
test$hour <- as.factor(test$hour)
test$wd <- as.factor(test$wd)


set.seed(888)
fitControl <- trainControl(method='cv', # 10-fold CV
                           10) # repeated ten times
Grid <- expand.grid(
  n.trees = c(500),
  interaction.depth = c(22) ,
  shrinkage = 0.2)
# boosted tree model
gbmFit1 <- train(count ~ ., data=train, method='gbm', trControl = fitControl, verbose=T,
                 tuneGrid = Grid)
gbmFit1
pred1 <- predict(gbmFit1, train)

compare <- data.frame(row.names='msle')  
pred1a<-round(pred1) 
pred1a[pred1a<=0] <- 1
compare$gbm <- rmsle(train$count, pred1a)


# fit 2 poisson
eGrid <- expand.grid(.alpha = (1:10) * 0.1, .lambda = "all")
Control <- trainControl(method = "repeatedcv",repeats = 3,verboseIter =TRUE)
fit2 <- train(count~., data= train,
              method = "glmnet",
              tuneGrid = eGrid,
              trControl = Control)
# random forest
tc <- trainControl("repeatedcv", number=10, repeats=10, classProbs=TRUE, savePred=T)
tc <- trainControl("cv",10, classProbs=TRUE, savePred=T)
fit3_2 <- train(count ~ . , data=train, method='rf', trControl=tc, preProc=c("center", "scale"),verbose=T)
pred3 <- predict(fit3_2, train)
pred3a<-round(pred3) 
pred3a[pred3a<=0] <- 1
compare$rf <- rmsle(train$count, pred3a)

# test set preProcess
# predictions
predictions <- predict(fit3, test)
gbmPrinted <- data.frame(test$datetime, predictions)
names(gbmPrinted)<- c('datetime', 'count')
gbmPrinted[which(gbmPrinted$count < 0),'count'] <- 0
#write.table(x=gbmPrinted, file='fr_cv_10.csv', sep=',', row.names=F)



