library(mlr)
library(tidyr)
library(dplyr)
library(assertthat)


normalizeDf <- function(obj) obj %>% normalizeFeatures(method="range", range=c(-1,1))


multipleTargets <- function(df, taskName, targets){
  
  plyr::llply(targets, function(tt){
    toDrop <- setdiff(targets, tt)
    dfSubset <- df %>%  dplyr::select(-one_of(toDrop)) %>% normalizeDf()
    makeRegrTask(data=dfSubset, target = tt, id = paste(taskName,tt,sep="-")) 
  })
}


#' Creates regression tasks for concrete slump data
#' 
#' \url{https://archive.ics.uci.edu/ml/datasets/Concrete+Slump+Test}
#' @return a list of 3 tasks - one for each target attribute
concreteSlumpTasks <- function(){
  df <- readr::read_csv('data/slump_test.data.csv') %>% dplyr::select(-No) %>% as.data.frame()
  assert_that(are_equal(nrow(df), 103))
  assert_that(are_equal(ncol(df), 10))

  targets <- c("SLUMP","COMPRESSIVE_STRENTH","FLOW")
  multipleTargets(df, "concrete_slump", targets) 
} 

#' Creates regression task for "Auto MPG" data
#' 
#' \url{https://archive.ics.uci.edu/ml/datasets/Auto+MPG}
#' @return the regression task
autoMpgTask <- function(){
  df <- readr::read_csv('data/auto-mpg.data.csv',na = '?') %>% 
      dplyr::select(-car_name) %>% as.data.frame() %>%
      dummies::dummy.data.frame(names=c("origin"), sep="_") %>%
      dplyr::mutate(horsepower = ifelse(is.na(horsepower), 0, horsepower)) %>%
      normalizeDf()
  
  assert_that(are_equal(nrow(df), 398))
  
  makeRegrTask(data=df, target = "mpg", id = "auto_mpg")
} 


#' Creates regression task for "Boston" data
#' 
#' \url{https://archive.ics.uci.edu/ml/datasets/Housing}
#' @return the regression task
bostonTask <- function(){
  assert_that(are_equal(nrow(MASS::Boston), 506))
  assert_that(are_equal(ncol(MASS::Boston), 14))
  
  makeRegrTask(data=MASS::Boston %>% normalizeDf(), target = "medv", id = "boston") 
} 



#' Creates regression task for "Wine quality (white)" data
#' 
#' \url{http://archive.ics.uci.edu/ml/datasets/Wine+Quality}
#' @return the regression task
whiteWineTask <- function(){
  df <- readr::read_csv2('data/winequality-white.csv',na = '?') %>% 
    dplyr::mutate_all(dplyr::funs(as.numeric)) %>% as.data.frame()
  
  assert_that(are_equal(nrow(df), 4898))
  assert_that(are_equal(ncol(df), 12))
  
  makeRegrTask(data=df%>% normalizeDf(), target = "quality", id = "white_wine")  
} 


#' Creates regression task for "California housing" data
#' 
#' \url{http://www.dcc.fc.up.pt/~ltorgo/Regression/DataSets.html}
#' @return the regression task
californiaTask <- function(){
  df <- readr::read_csv('data/cal_housing.csv',na = '?') %>% as.data.frame()
  
  assert_that(are_equal(nrow(df), 20640))
  assert_that(are_equal(ncol(df), 9))
  
  makeRegrTask(data=df%>% normalizeDf(), target = "medianHouseValue", id = "california_housing")  
} 




