#' library(mlr)
#' library(tidyr)
#' library(dplyr)
#' library(assertthat)
#' 
#' 
#' normalizeDf <- function(obj) obj %>% normalizeFeatures(method="range", range=c(-1,1))
#' 
#' 
#' multipleTargets <- function(df, taskName, targets){
#'   
#'   plyr::llply(targets, function(tt){
#'     toDrop <- setdiff(targets, tt)
#'     dfSubset <- df %>%  dplyr::select(-one_of(toDrop)) %>% normalizeDf()
#'     makeRegrTask(data=dfSubset, target = tt, id = paste(taskName,tt,sep="-")) 
#'   })
#' }
#' 
#' 
#' #' Creates regression tasks for concrete slump data
#' #' 
#' #' \url{https://archive.ics.uci.edu/ml/datasets/Concrete+Slump+Test}
#' #' @return a list of 3 tasks - one for each target attribute
#' concreteSlumpTasks <- function(){
#'   df <- readr::read_csv('/Users/p.zawistowski/Priv/experiments/data/slump_test.data.csv') %>% dplyr::select(-No) %>% as.data.frame()
#'   assert_that(are_equal(nrow(df), 103))
#'   assert_that(are_equal(ncol(df), 10))
#'   
#'   targets <- c("SLUMP","COMPRESSIVE_STRENTH","FLOW")
#'   multipleTargets(df, "concrete_slump", targets) 
#' } 
#' task = concreteSlumpTasks()[[2]]
#' 
#' 
#' scalOptim = function(goal, n, optim.params){
#'     res <- cmaes::cma_es(runif(n, min=optim.params$par.min, max=optim.params$par.max),
#'                          goal,
#'                          lower=rep(optim.params$par.min,n),
#'                          upper=rep(optim.params$par.max,n), 
#'                          control=list(stopfitness=optim.params$stop.fitness))                      
#'     res
#' }
#' 
#' #train(makeLearner('regr.IBHM'),task%>%subsetTask(features=c('Cement','Fly_ash','Water')))