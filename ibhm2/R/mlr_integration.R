makeRLearner.regr.IBHM = function() {
  makeRLearnerRegr(
    cl = "regr.IBHM",
    package = "IBHM",
    par.set = makeParamSet(
      makeDiscreteVectorLearnerParam(id = "scal.candidates",
      values = c('dot.pr','radial','root.radial'),
      default = list('dot.pr','radial'))
      , makeDiscreteVectorLearnerParam(id = "activ.candidates",
      values = c('tanh','logsig','lin'),
      default = list('tanh','logsig','lin'))
      , makeNumericLearnerParam("stop.criterion.frac", default = 0.1, lower = 0.01, upper = 0.99)
      , makeIntegerLearnerParam("stop.criterion.max.failures", lower=1, upper = 10, default = 2)
      , makeNumericLearnerParam("stop.criterion.approximate.timeout", default = Inf, lower = 1.0, upper = Inf, allow.inf = TRUE)
      , makeNumericLearnerParam("scal.lambda", default = 0.1, lower = 0.0, upper = 1.0)
      , makeFunctionLearnerParam("weighting.function", default = function(y, w.par){ 0.01+dnorm(y,sd=abs(w.par))})
      , makeIntegerLearnerParam("optim.maxit", lower=10, upper = 100000, default = 50)
      , makeIntegerLearnerParam("optim.retries", lower=1, upper = 10, default = 3)
      , makeIntegerLearnerParam("final.estimation.maxit", default = 50)
      , makeLogicalLearnerParam("verbose", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics"),
    name = "Incrementally Built Heterogenous Model",
    short.name = "IBHM",
    note = ""
  )
}


trainLearner.regr.IBHM = function(.learner, .task, .subset, .weights = NULL, ...) {
  res <- getTaskData(.task,.subset, target.extra = TRUE)
  IBHM::TrainIBHM(x = res$data,
                  y = res$target, 
                  config = ConfigureIBHM(...))
}

predictLearner.regr.IBHM = function(.learner, .model, .newdata, ...) {
  yh <- predict(.model$learner.model, as.matrix(.newdata))
  as.vector(yh)
}