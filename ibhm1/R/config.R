
# IBHM configuration ------------------------------------------------------

ConfigureIBHM1  <- function(stop.criterion.frac = 0.15,
                           weighting.function = function(y, w.par){ 0.01+dnorm(y,sd=abs(w.par))},
                           scal.optim = 'DE',
                           scal.optim.params = list(retries=3, maxit=50, stop.fitness=0),
                           scal.candidates = c('dot.pr','radial','root.radial'),                                                      
                           activ.optim = 'DE',
                           activ.optim.params = list(retries=3, maxit=100, stop.fitness = 0),
                           activ.candidates = c('tanh','logsig','lin'),
                           jit=TRUE,                           
                           verbose=FALSE,
                           final.estimation = 'all',
                           final.estimation.x = NULL,
                           final.estimation.y = NULL,
                           final.estimation.maxit = 100
){    
  list( sc.frac = stop.criterion.frac,
        wf = weighting.function,
        final.estimation = switch(final.estimation,
                                  weights = OptimizeAllWeights,
                                  all = OptimizeAllParams,
                                  stop('Unknow final.estimation type: ',final.estimation)),
        final.estimation.x = final.estimation.x,
        final.estimation.y = final.estimation.y,
        final.estimation.maxit = final.estimation.maxit,
        jit=jit,
        verbose=verbose,
        scal = list(                  
          optim = switch(scal.optim, 
                         CMAES = OptimizeCMAES,
                         DE = OptimizeDE,
#                          GA  = OptimizeGA,
                         NM = OptimizeNM,
                         PSO  = OptimizePSO,                         
                         stop('Unknown scal optimization method: ',activ.optim)),
          optim.params = scal.optim.params,
          candidates = ScalFunctions(scal.candidates),
          evaluator = NULL
        ),
        activ = list(          
          optim = switch(activ.optim, 
                         CMAES = OptimizeCMAES,
                         DE = OptimizeDE,
#                          GA = OptimizeGA,
                         NM = OptimizeNM,
                         PSO  = OptimizePSO,
                         stop('Unknown activ optimization method:',activ.optim)),
          optim.params = activ.optim.params,
          candidates = ActivationCandidates(activ.candidates)
        )
  )
}