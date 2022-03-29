library(geoR)
library(PrevMap)
vari.time <- function(time,...) {
  variog(coords=cbind(time,1),...)
}

vari.env <- function(variogram.t,time,...) {
  variog.mc.env(coords=cbind(time,1),
                obj.variog = variogram.t,...) 
}


fit.matern <- function(form,...,data,time) {
  data$t_aux <- 1
  out <- linear.model.MLE(formula=form,...,data=data,
                   coords=as.formula(paste("~",time,"+ t_aux")))
  out$call$formula <- form
  out
}


time.predict <- function(fitted.model,predictors=NULL,time.pred,scale.pred=NULL,...) {
  if(scale.pred=="exponential") {
    scale.pred <- "odds"
  } else {
    scale.pred <- "logit"
  }
  time.pred <- cbind(time.pred,1)
  fitted.model$call$formula
  out <- spatial.pred.linear.MLE(object=fitted.model,
                          predictors = predictors,
                          grid.pred = time.pred,
                          scale.predictions = scale.pred,...,
                          messages=FALSE)
  if(scale.pred=="odds") {
    out$predictions <- out$odds$predictions
    out$quantiles <- out$odds$quantiles
  } else {
    out$predictions <- out$logit$predictions
    out$quantiles <- out$logit$quantiles
  }
  out$time.pred <- time.pred[,1]
  out
}


create.lag.var <- function(x,lag,data) {
  tlag <- data$t-lag
  ind.lag <- sapply(tlag,function(x) {
    out <- which((data$t-x)==0)
    if(length(out)==0) out <- NA
    return(out)
  })
  x[ind.lag]
}


