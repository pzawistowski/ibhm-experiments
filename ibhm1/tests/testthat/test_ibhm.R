library(IBHM1)
library(foreach)

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
  x <- seq(-6,6,length.out=100)
  y <- tanh(x)
  
  set.seed(8)  
  m <- TrainIBHM1(x,y, ConfigureIBHM1(scal.candidates='dot.pr', activ.candidates='tanh'))  
  expect_true(summary(m)$rmse<0.1) 
})


test_that("lin activ function works",{
  x <- seq(-6,6,length.out=100)
  y <- 2*x 
  
  set.seed(8)  
  m <- TrainIBHM1(x,y, ConfigureIBHM1(scal.candidates='dot.pr', activ.candidates='lin'))
  
  expect_true(summary(m)$rmse<0.1) 
})


test_that("tanh2d",{  
  x <- mesh(seq(-3,3,length=10),2)
  y <- tanh(2*(x%*%c(0.1,0.5))+1) + 1
      
  set.seed(8)  
  m <- TrainIBHM1(x,y,ConfigureIBHM1(scal.candidates='dot.pr', activ.candidates='tanh'))
  
  expect_true(summary(m)$rmse<0.1) 
})

test_that("final estimation works",{  
  x <- mesh(seq(-3,3,length=10),2)
  y <- tanh(2*(x%*%c(0.1,0.5))+1) + 1
  
  set.seed(8)  
  m <- TrainIBHM1(x[1:30,],y[1:30],ConfigureIBHM1(scal.candidates='dot.pr', activ.candidates='tanh', final.estimation.x = x, final.estimation.y = y))
  
  expect_true(summary(m)$rmse<0.2) 
})


test_that("scal optim params",{  
  x <- seq(-6,6,length.out=100)
  y <- 2*tanh(x)
  
  set.seed(8)  
  
  optims <- list('DE','CMAES', 'NM', 'PSO')
  
  for(opt in optims){
    conf <- ConfigureIBHM1(scal.candidates='dot.pr', activ.candidates='tanh',
                          scal.optim = opt, activ.optim = opt,
                          scal.optim.params = list(retries=2, maxit=50, stop.fitness=0),
                          activ.optim.params = list(retries=2, maxit=50, stop.fitness=0),                      
    )
    m <- TrainIBHM1(x,y, conf)
    
    expect_true(summary(m)$rmse<0.1) 
  }
  
})






