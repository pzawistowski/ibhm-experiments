MultiOptim <- function(retries, optim.fun, ...){
  best <- list(eval=Inf)
  for(i in 1:retries){
    res <- optim.fun(...)
    if(res$eval < best$eval){
      best <- res
    }
  }
  
  res
}

# Function optimization ---------------------------------------

OptimizeCMAES <- function(goal, n, optim.params){
  MultiOptim(optim.params$retries, function(){    
      res <- cmaes::cma_es(runif(n,min=optim.params$par.min,max=optim.params$par.max),goal,lower=rep(optim.params$par.min,n),upper=rep(optim.params$par.max,n), control=list(maxit=optim.params$maxit, stopfitness=optim.params$stop.fitness))                      
      list(eval=res$value, par=res$par)
  })
}


OptimizeNM <- function(goal, n, optim.params){
  MultiOptim(optim.params$retries, function(){
    ctrl <- list(maxit=optim.params$maxit)
    res <- optim(runif(n,min=optim.params$par.min,max=optim.params$par.max),goal, control=ctrl)  
    
    list(eval=res$value, par=res$par)
  })
}

OptimizeDE <- function(goal, n, optim.params){
  MultiOptim(optim.params$retries, function(){  
    ctrl <- DEoptim::DEoptim.control(itermax=optim.params$maxit, VTR=optim.params$stop.fitness, trace=FALSE)
    res <- DEoptim::DEoptim(goal,lower=rep(optim.params$par.min,n),upper=rep(optim.params$par.max,n), ctrl)                  
    
    list(eval=res$optim$bestval, par=res$optim$bestmem)
  })
}

OptimizePSO <- function(goal, n, optim.params){
  MultiOptim(optim.params$retries, function(){  
    control <- list(maxit=optim.params$maxit, trace = FALSE, abstol = optim.params$stop.fitness)
    out <- pso::psoptim(rep(NA, n), goal, lower=rep(optim.params$par.min,n),upper=rep(optim.params$par.max,n), control=control)

    list(eval=out$value, par=out$par)
  })
}

# OptimizeGA <- function(goal, n, optim.params){
#   MultiOptim(optim.params$retries, function(){      
#     f <- function(par) -goal(par)        
#     
#     out <- GA::ga(type = "real-valued", fitness = f, min=rep(optim.params$par.min,n),
#               max=rep(optim.params$par.max,n), maxiter=optim.params$maxit, popSize=10*n, maxfitness=-1*optim.params$stop.fitness,
#               monitor=NULL, selection=GA::gareal_rwSelection, crossover=GA::gareal_blxCrossover)
#     
#     list(eval=out@fitnessValue, par = out@solution[1,])    
#   })
# }
