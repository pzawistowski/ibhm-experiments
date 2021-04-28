set.seed(123)
library(plyr)
source('mlr_tasks.R')
source('mlr_learners.R')
library("parallelMap")


tuningMeasures <- list(rmse)
tuningResampling <- cv3
tuningControl <- makeTuneControlRandom(budget = 100)


outerResampling <- makeResampleDesc("CV", iters=10, predict = "both")
outerMeasures <- list(setAggregation(rmse, test.mean),
                      setAggregation(rmse, test.sd), 
                      setAggregation(rmse, train.mean), 
                      setAggregation(rmse, train.sd), 
                      setAggregation(timetrain, train.mean), 
                      setAggregation(timetrain, train.sd)
)

learners <- list( ibhm1Learner()
  , ibhm2Learner()
  , xgboostLearner(tuningResampling, tuningMeasures, tuningControl)
  , lmLearner(tuningResampling, tuningMeasures, tuningControl)
  , nnetLearner(tuningResampling, tuningMeasures, tuningControl)
  , svmLearner(tuningResampling, tuningMeasures, tuningControl)
)

tasks <- c(concreteSlumpTasks(), list(autoMpgTask(), bostonTask(), whiteWineTask(), californiaTask()))


doRun <- function(learners, tasks){
  parallelStartMulticore(level = 'mlr.resample')
  
  runTime <- format(Sys.time(), "%Y-%m-%d_%H.%M.%S")
  benchmarkRes <- plyr::llply(learners, function(learner){
    plyr::llply(tasks, function(task){
      res <- mlr::benchmark(list(learner), list(task), outerResampling, outerMeasures)
      saveRDS(res, paste(learner$id,task$task.desc$id,runTime,'RDS',sep="."))
      
      res
    })
  })
  parallelStop()
  benchmarkRes
}