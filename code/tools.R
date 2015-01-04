#submission tool
write.submission <- function(model, name, tt){
  predict.model <- predict(model, tt)
  #colnames(predict.model) = 'count'
  #build a dataframe with our results
  submit <- data.frame(datetime = tt$datetime, count=predict.model)
  #write results to .csv for submission
  write.csv(submit, file=name,row.names=FALSE,quote=FALSE)
}

#performance comparison
tool.performance <- function(fit.model,subTest,subTrain,name.=name,compare.=compare,check.lgcount=FALSE,check.update=TRUE){
  
  if(check.lgcount){
    predictT.model.testing <- predict(fit.model, newdat=subTest)
    predictT.model.testing<- exp(predictT.model.testing)-1
    predictT.model.Training <- predict(fit.model, newdat=subTrain)
    predictT.model.Training<- exp(predictT.model.Training)-1
    subTest$count <- as.numeric(exp(subTest$lgcount)-1)
    subTrain$count <- as.numeric(exp(subTrain$lgcount)-1)
    
  } else{ 
    predictT.model.testing <- predict(fit.model, newdat=subTest)
    if (any(predictT.model.testing<0)) {
      predictT.model.testing[predictT.model.testing<0] <- 0
    }
    predictT.model.Training <- predict(fit.model, newdat=subTrain)
    if (any(predictT.model.Training<0)) {
      predictT.model.Training[predictT.model.Training<0] <- 0
    }  
  }
  
  rmsle.Test <- rmsle(subTest$count,predictT.model.testing)
  rmse.Test <- rmse(subTest$count,predictT.model.testing)
  rmsle.Train <- rmsle(subTrain$count,predictT.model.Training)
  rmse.Train <- rmse(subTrain$count,predictT.model.Training)
  
  diff <- summary(as.numeric(subTrain$count - predictT.model.Training))
  diff.Test <- summary(as.numeric(subTest$count - predictT.model.testing))
  ll <- list('Test.rmsle'=rmsle.Test, 'Test.rmse'=rmse.Test, 'Train.RMSLE'=rmsle.Train, 
             'Train.RMSE'=rmse.Train,'Test.diff.median'=as.numeric(diff.Test[3]),'Test.diff.mean'=as.numeric(diff.Test[4]),
             'Train.diff.median'=as.numeric(diff[3]),'Train.diff.mean'=as.numeric(diff[4]))
  if (check.update) {
    compare.<-rbind(compare., mmm=ll)
    rname <- rownames(compare.)
    rname[nrow(compare.)] <- name.  
    rownames(compare.) <- rname 
  } else {
    compare. <- as.data.frame(ll,row.names=name.)
  }
  compare.
}

