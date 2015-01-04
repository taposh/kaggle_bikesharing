##DEFINE Metric function

mymetric <- function(data, lev=NULL, model=NULL){
  require(Metrics)
  pred<-data[,'pred']
  obs<-data[,'obs']
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  
    if (length(obs) + length(pred) == 0) {
      out <- rep(NA, 2)
    }
    else {
      if (length(unique(pred)) < 2 || length(unique(obs)) < 
            2) {
        resamplCor <- NA
      }
      else {
        resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), 
                          silent = TRUE)
        if (class(resamplCor) == "try-error") 
          resamplCor <- NA
      }
      
#      msle<-msle(obs, pred)
      rmsle<-rmsle(obs,pred)
#      rmse<-rmse(obs,pred)
      out <- rmsle
    }
    names(out) <- "RMSLE"

  if (any(is.nan(out))) 
    out[is.nan(out)] <- NA
  out
  
}

my2metric <- function(data, lev=NULL, model=NULL){
  pred<-data[,'pred']
  obs<-data[,'obs']
  rmse <- rmse(pred,obs)
  isNA <- is.na(pred)
  pred[isNA] <- 0
  obs[isNA] <- 0
  pred[which(pred < 0)] <- 0.0
  obs[which(pred < 0)] <- 0.0 
  rmsle <- rmsle(obs,pred)
  out <- c(rmsle,rmse)
  names(out) <- c("RMSLE","RMSE")
  
  if (any(is.nan(out))) 
    out[is.nan(out)] <- 0.0
  out
  
}

