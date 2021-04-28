context("MLR integration")


test_that("a simple smoke test passes", {
  # Given
  set.seed(1)
  x <- seq(-6,6,length.out=100); y <- tanh(x) 

  task <- makeRegrTask(data=data.frame(x,y), target = "y") 
  lrn <- makeLearner("regr.IBHM")
  
  # When
  m <- train(lrn, task)
  
  # Then
  yh <- predict(m, task)$data$response
  expect_true(mean(abs(y-yh)) < 0.1)
})



test_that("the method's parameters are passed correctly", {
  # Given
  set.seed(123)
  x <- seq(-6,6,length.out=100); y <- tanh(x) 
  task <- makeRegrTask(data=data.frame(x,y), target = "y") 
  wf <- function(y, w.par){dnorm(y,sd=abs(w.par))}
  op <- list(retries=3, maxit=50, stop.fitness=0)
  
  # When
  lrn <- makeLearner("regr.IBHM") %>% 
    setHyperPars( weighting.function = wf
                   , scal.candidates = list('dot.pr')
                   , activ.candidates = list('tanh')
                   , final.estimation.maxit = 1
          )
  
  m <- train(lrn, task)
  
  # Then
  extracted  <- m$learner.model$train$config
  expect_identical(extracted$weighting.function, wf)
  expect_identical(extracted$scal.candidates, list('dot.pr'))
  expect_identical(extracted$activ.candidates, list('tanh'))
  expect_identical(extracted$final.estimation.maxit, 1)
  
})




