context("MLR integration")


test_that("a simple smoke test passes", {
  # Given
  x <- seq(-6,6,length.out=100); y <- tanh(x)
  task <- makeRegrTask(data=data.frame(x,y), target = "y") 
  lrn <- makeLearner("regr.IBHM1")
  
  # When
  m <- train(lrn, task)
  
  # Then
  yh <- predict(m, task)$data$response
  expect_true(mean(abs(y-yh)) < 0.01)
})

