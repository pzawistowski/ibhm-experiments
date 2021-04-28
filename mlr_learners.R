
library(mlr)

lmLearner <- function(resampling = cv3
           , measures = list(rmse)
           , control = makeTuneControlRandom(budget = 10)){
  ps <- makeParamSet(
    makeNumericParam("tol", default=0.0000001, lower=0.0, upper=1.0)          
  )  
  
  makeLearner("regr.lm") %>% 
    makeTuneWrapper(resampling, measures, ps, control)
}


xgboostParams <- function(){
  makeParamSet(
    makeNumericParam("eta", default=0.3, lower=0.1, upper=1.0)          
    , makeNumericParam("gamma", default=0, lower=0, upper=1.0)            
    , makeIntegerParam("max_depth", default=6, lower=3, upper=10)         
    , makeNumericParam("min_child_weight", default=1, lower=0, upper=2)   
    , makeNumericParam("subsample", default=1, lower=0.5, upper=1)        
    , makeNumericParam("colsample_bytree", default=1, lower=0.5, upper=1) 
    , makeNumericParam("colsample_bylevel", default=1, lower=0.5, upper=1)
    , makeNumericParam("lambda", default=0, lower=0, upper=1)             
    , makeNumericParam("alpha", default=0, lower=0, upper=1)              
    , makeIntegerParam("nrounds", default=1000, lower=1, upper=50000)          
  )
}

xgboostLearner <- function(resampling, measures, control){
  
  makeLearner("regr.xgboost") %>% 
    setHyperPars(booster="gbtree", nthread = 1) %>% 
    makeTuneWrapper(resampling, measures, xgboostParams(), control)
}
  

nnetLearner <- function(resampling, measures, control){
  
  ps <-  makeParamSet(
    makeIntegerParam("size",        default=3, lower = 1, upper=20)
  , makeIntegerParam("maxit",       default=100, lower = 20, upper=10000)
  , makeNumericParam("decay",       default=0, lower=0, upper=20)
  , makeNumericParam("abstol",      default=0.0001, lower=0, upper=1)
  , makeNumericParam("reltol",      default=1e-08, lower=0, upper = 1 )
  )
  
  makeLearner("regr.nnet", skip = TRUE) %>% 
    makeTuneWrapper(resampling, measures, ps, control)
}


svmLearner  <- function(resampling, measures, control){
  
  ps <-  makeParamSet(
      makeIntegerParam("nu",       lower = -10, upper=0, trafo=exp)
    , makeIntegerParam("epsilon",  lower = 0.0, upper=1)
  )
  
  makeLearner("regr.ksvm") %>% 
    makeTuneWrapper(resampling, measures, ps, control)
}


ibhm1Learner <- function(){

  require(IBHM1)  
  makeLearner("regr.IBHM1", id = "IBHM_1") %>% setHyperPars( verbose = FALSE) 
}


ibhm2Learner <- function(){
  sqr <- function(y,...){
     w <- y*0 + 0.0001
     w[abs(y)<0.1] <- 1
     w
  }
  require(IBHM)
  makeLearner("regr.IBHM", id = "IBHM_2") %>% 
      setHyperPars(final.estimation.maxit = 20
                   , stop.criterion.max.failures = 1
                   , verbose = FALSE 
                   , scal.lambda = 1e-9
                   , optim.retries = 3
                   , weighting.function = sqr)
}


