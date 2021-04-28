context("IBHM")


mesh <- function(x,dim){  
  nrows <- length(x)^(dim)
  out <- array(dim=nrows*dim)
  idx = 1;
  for(d in 1:dim){
    r = length(x)^(dim-d) 
    while(idx < d*nrows){
      for(v in x){
        out[idx:(idx+r-1)] = v
        idx = idx + r;
      }
    }
  }
  dim(out) <- c(nrows,dim)
  return(out)
}

test_that("tanh activ function works", {
  # Given
  x <- seq(-6,6,length.out=100)
  y <- tanh(x)
  set.seed(8)  
  
  # When
  m <- TrainIBHM(x,y, ConfigureIBHM(scal.candidates='dot.pr', activ.candidates='tanh', verbose = T))  
  
  # Then
  expect_true(summary(m)$rmse<0.2)
})



test_that("logistic activ function works", {
  # Given
  x <- seq(-6,6,length.out=100)
  y <- tanh(x)
  set.seed(10)
  
  # When
  m <- TrainIBHM(x,y, ConfigureIBHM(scal.candidates='dot.pr', activ.candidates='logsig',  verbose = T))  
  
  # Then
  expect_true(summary(m)$rmse<0.1)
})


test_that("linear activ function works", {
  # Given
  x <- seq(-6,6,length.out=100)
  y <- 2*x 
  set.seed(8)
  
  # When
  m <- TrainIBHM(x,y, ConfigureIBHM(scal.candidates='dot.pr', activ.candidates='lin'))
  
  # Then
  expect_true(summary(m)$rmse<0.1)
})





