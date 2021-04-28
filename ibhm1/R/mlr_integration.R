makeRLearner.regr.IBHM1 = function() {
  makeRLearnerRegr(
    cl = "regr.IBHM1",
    package = "IBHM1",
    par.set = makeParamSet(
      makeDiscreteVectorLearnerParam(id = "scal.candidates", values = c('dot.pr','radial','root.radial'), default = list('dot.pr','radial'))
      , makeFunctionLearnerParam("weighting.function", default = function(y, w.par){ 0.01+dnorm(y,sd=abs(w.par))})
      , makeLogicalLearnerParam("verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics"),
    name = "Incrementally Built Heterogenous Model",
    short.name = "IBHM1",
    note = ""
  )
}


trainLearner.regr.IBHM1 = function(.learner, .task, .subset, .weights = NULL, ...) {
  res <- getTaskData(.task,.subset, target.extra = TRUE)
  IBHM1::TrainIBHM1(x = res$data,
                  y = res$target, 
                  config = IBHM1::ConfigureIBHM1(...))
}

predictLearner.regr.IBHM1 = function(.learner, .model, .newdata, ...) {
  yh <- predict(.model$learner.model, as.matrix(.newdata))
  as.vector(yh)
}