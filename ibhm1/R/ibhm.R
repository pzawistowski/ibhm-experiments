source('R/scals.R')
source('R/activs.R')
source('R/optim.R')
source('R/stopCriteria.R')
source('R/config.R')

loadModule("evaluator",what=T)

# IBHM method -------------------------------------------------------------

TrainIBHM1 <- function(x,y, config = ConfigureIBHM1()  ){
  if(config$jit){    
    enableJIT(3)
  }  
  
  ctx <- CreateContext(x,y,config)
  ctx$log(str(ctx))
  
  ctx$m$par$w0 <- mean(y)  
  ctx$yr <- ctx$y-ctx$m$par$w0
  
  while(ctx$continue){      
    ctx <- ctx$sc$eval( DoIteration(ctx) )    
  }      
  
  ctx$log('Iterations finished RMSE: ', CalcRMSE(ctx))
  ctx$log('Final parameter estimation ')
  ctx$m$train$pre.final.model <- ctx$m
  ctx <- config$final.estimation(ctx, x = ctx$x.final, y = ctx$y.final, maxit = config$final.estimation.maxit)
  
  ctx$log('\nFinal RMSE: ',CalcRMSE(ctx))
  
  return(ctx$m)
}


DoIteration <- function(ctx){
  ctx$log('\nIteration ',ctx$iteration, ' RMSE: ', CalcRMSE(ctx))  
  ctx$iteration <- ctx$iteration + 1
  
  ctx <- FindActiv( FindScal(ctx) ) 
  ctx$m$par$w <- append(ctx$m$par$w,1)  
  ctx <- OptimizeWeights(ctx)
  UpdateResidual(ctx)
}

CalcRMSE <- function(ctx){
  sqrt(mean((ctx$y-predict.IBHM1(ctx$m,ctx$x))^2))
}



# Method runtime context --------------------------------------------------

CreateContext <- function(x,y, params){
  x <- as.matrix(x)
  y <- as.matrix(y)
  stopifnot(nrow(x)==nrow(y), nrow(x)>10, is.null(params$final.estimation.x) == is.null(params$final.estimation.y))  
  
  
  if(params$verbose){
    log <- function(...) cat(...,'\n')
  }else{
    log <- function(...) {}
  }
  
  x.m <- max(abs(x))
  params$scal$optim.params$par.min <- min(-1,-x.m)
  params$scal$optim.params$par.max <- max(1, x.m)
  
  params$activ$optim.params$par.min <- min(-1,-x.m)
  params$activ$optim.params$par.max <- max(1,x.m)
  
  scalCppEvaluator <- new(RcppScalEvaluator, y)
  if(is.null(params$scal$evaluator)){
    params$scal$evaluator <- function(zCandidate, w, ctx) scalCppEvaluator$evaluate(zCandidate,w)
  }   
  sc.mask <- sample(nrow(x),ceiling(nrow(x)*params$sc.frac))
  
  append(params,
         list( x = x,
               y = y, 
               yr = y,               
               x.final = if(is.null(params$final.estimation.x)) x else params$final.estimation.x,
               y.final = if(is.null(params$final.estimation.y)) y else params$final.estimation.y, 
               m=CreateEmptyModel(x,y, filtering.function = params$wf), 
               scal.y = list(),
               scal.cpp.evaluator = scalCppEvaluator,               
               continue=TRUE, 
               iteration=0,
               log = log,
               sc = ValidationSC1(as.matrix(x[sc.mask,]),as.matrix(y[sc.mask]))
         )
  )
}


# IBHM model structure ----------------------------------------------------
CreateEmptyModel <- function(x=NULL,y=NULL, filtering.function){
  m <- list(par = list(
                w0=0,
                w=vector(),
                d=list(),                            
                a=vector(),
                b=vector()),
            scals=list(),            
            activs=list(),            
            train=list(x=x,y=y, filtering.function = filtering.function, w.pars = vector()))
  class(m) <- 'IBHM1'
  return(m)
}



# Standard S3 methods -----------------------------------------------------
predict.IBHM1 <- function(object,x = NULL,...){  
  m<-object
  if(is.null(x)){
    x <- m$train$x
  }else{
    x <- as.matrix(x)  
  }  
  
  d <- dim(x)
  len <- length(m$scals)  
  y <- matrix(m$par$w0,d[[1]],1)
  
  
  if(len > 0){
    for(i in 1:len){
      act <- m$activs[[i]]
      a <- m$par$a[[i]]
      b <- m$par$b[[i]]
            
      scal <- m$scals[[i]]
      d <- m$par$d[[i]]
      
      w <- m$par$w[[i]]                       
      
      y <- y + w * act( a*scal(x,d)+b)
    }
  }
  
  y
}


length.IBHM1 <- function(x){
  length(x$par$w)
}

ToString <- function(m){  
  f <- function(v){
    sprintf('%.2e',v)
  }
  
  str <- f(m$par$w0)
  len <- length(m$par$w)
  
  if(len>0){  
    for(i in 1:length(m)){
      w <- m$par$w[[i]]
      if(w>0) s<-'+' else s<-''
      
      act <- expr(m$activs[[i]])
      a <- f(m$par$a[[i]])
      b <- f(m$par$b[[i]])
      
      scal <- expr(m$scals[[i]])
      d <- paste(f(m$par$d[[i]]),collapse=' ')
      
      
      str <- paste(str,s,f(w),act,' (',a,'*',scal,'(x,[',d,'])','+',b,') ')
    }
  }
  
  str
  
}

summary.IBHM1 <- function(object,...){  
  y <- object$train$y
  x <- object$train$x
  
  se <- c((y - predict(object))^2)
  
  res <- list(model = ToString(object),
              model.size = length(object),
              TrainSize = nrow(y),
              TrainDim = ncol(x),
              mse = mean(se),
              sd.se = sd(se),
              rmse = sqrt(mean(se)),
              cor=cor(predict(object),y))
  class(res) <- 'summary.IBHM1'
  res
}

print.summary.IBHM1 <- function(x,...){
  cat('Model equation: ', x$model,'\n',
      'Model size: ',x$model.size,'\n\n',
      'Train set dim: ', x$TrainDim, ' Train set size: ', x$TrainSize,'\n',      
      'MSE:  ', x$mse, ' Std. dev:', x$se.sd,'\n',
      'RMSE: ', x$rmse, '\n',
      'Pearson correlation coefficient: ', x$cor,'\n')
}


# Finding a scalarization function ----------------------------------------

FindScal <- function(ctx){
  within(ctx,{
    log(' Finding next scal')
                
    best <- list(scal=NULL, eval = Inf, d = NULL)
    
    for( s in  scal$candidates){
      candidate <- OptimizeScal(s,ctx)
      log('  ',expr(s), ' eval : ', candidate$eval)
      
     # dz <- candidate$scal(x, candidate$d) - ctx$yr      
    #  candidate$eval <- mean(dz^2)
      if(candidate$eval < best$eval){
        best <- candidate        
      }
    }
    
    log(' Best scal: ',expr(best$scal), ' d: ', best$d,' w.par:',best$w.par)
        
    m$scals[[iteration]]  <- best$scal
    m$par$d[[iteration]] <- best$d
    m$train$w.pars[[iteration]] <- best$w.par
    
    zk <- best$scal(x, best$d)
    w <- wf(zk, best$w.par)   
    scal.cpp.evaluator$addScal(zk)    
  })
}


OptimizeScal <- function(scal, ctx){
  
  goal <- function(par){              
    w.par <- par[[1]]
    zCandidate <- scal(ctx$x, par[-1])
    w <- ctx$wf(zCandidate, w.par)
              
    return(1-ctx$scal$evaluator(zCandidate,w,ctx))
  }
  n <- ScalParamDim(scal,ctx$x) + 1    
  res <- ctx$scal$optim(goal,n,ctx$scal$optim.params)    
  
  list(scal = scal, eval = res$eval, w.par=res$par[[1]], d=res$par[-1])
}

# Finding an activation function #####################


FindActiv <- function(ctx){
  within(ctx, {
    log(' Finding next activ')    
    
    zk <- RunLastScal(ctx)
    best <- NULL
    best.eval <- Inf
      
    for(i in 1:length(activ$candidates)){
      cand <- OptimizeActiv(activ$candidates[[i]], zk, ctx)        
      if(cand$eval < best.eval){
        best <- cand      
        best.eval <- cand$eval
      }
    }    
          
    m$activs[[iteration]] <- best$activ
    m$par$a[[iteration]]  <- best$a
    m$par$b[[iteration]]  <- best$b  
    
    log(' Best activ: ', expr(best$activ), ' a:',best$a, ' b:',best$b)    
  })
}


OptimizeActiv <- function(activ, zk, ctx){  
  goal <- function(par){          
    a <- par[[1]]
    b <- par[[2]]
    
    vk <- activ(a*zk+b)    
    
    eval <- 1-abs(EvaluateActiv(ctx$yr,vk,ctx$w))     
    
    if(is.nan(eval)){  
      eval <- Inf        
    } 
    return(eval)
  }  
  
  res <- ctx$activ$optim(goal, 2, ctx$activ$optim.params)  
      
  ctx$log('  ',expr(activ),' eval:',res$eval)
  list(activ=activ, eval=res$eval,  a=res$par[[1]], b=res$par[[2]])
}

EvaluateActiv <- function(yr,vk,w){
  val<-weightedR(yr, vk, w)  
  
  if(is.nan(val) || !is.finite(val)){
    val<-0
  }
  
  return(val)
}

RunLastScal <-function(ctx){
  scal <- ctx$m$scals[[ctx$iteration]]  
  d <- ctx$m$par$d[[ctx$iteration]]
  
  scal(ctx$x,d)  
}

RunLastActiv <-function(ctx){  
  activ<- ctx$m$activs[[ctx$iteration]]  
  a <- ctx$m$par$a[[ctx$iteration]]
  b <- ctx$m$par$b[[ctx$iteration]]
  
  activ(a*RunLastScal(ctx)+b)
}



# Estimating regression weights -------------------------------------------

OptimizeWeights <- function(ctx){      
  vk <- RunLastActiv(ctx)  
  vk_bar <- vk - weightedMean(vk,ctx$w)
  
  yr_bar <- ctx$yr - weightedMean(ctx$yr,ctx$w)
  
  goal <- function(w){                    
    return(weightedMean((yr_bar - w*vk_bar)^2 ,ctx$w) )                
  }
  
  res <- optim(1,goal, method="BFGS")  
  
  ctx$m$par$w[[ctx$iteration]] <- res$par
  ctx$m$par$w0<- ctx$m$par$w0 - res$par*mean(vk)
  
  ctx$log(' w: ', res$par,' w0: ', ctx$m$par$w0, ' dw0: ', -mean(vk))
  
  ctx
}

OptimizeAllWeights <- function(ctx, x = ctx$x, y = ctx$y, maxit=100){  
  within(ctx,{
    log(' Optimizing all weights')
    
    goal <- function(par){    
      m$par$w0 <- par[[1]]
      m$par$w <- par[2:length(par)]
      
      return(mean((y - predict.IBHM1(m, x))^2))
    }
    
    res <- optim(c(m$par$w0, m$par$w),goal, method="BFGS", control=list(maxit=maxit, trace=F))
    
    m$par$w0 <- res$par[[1]]
    m$par$w <- res$par[2:length(res$par)]        
  })    
}

OptimizeAllParams <- function(ctx,x = ctx$x, y = ctx$y, maxit=200){  
  within(ctx,{
    log(' Optimizing all parameters')
    
    goal <- function(par){          
      m$par <- relist(par, m$par)        
      return(mean((y - predict.IBHM1(m,x))^2))
    }
              
    res <- optim(unlist(m$par),goal, method="BFGS", control=list(maxit=maxit, trace=F))  
    m$par <- relist(res$par, m$par)          
  })  
}

UpdateResidual <- function(ctx){
  ctx$yr <- ctx$y - predict.IBHM1(ctx$m, ctx$x)
  ctx$scal.cpp.evaluator$updateResidual(ctx$yr)
  ctx
}





