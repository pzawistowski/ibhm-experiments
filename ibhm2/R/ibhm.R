source('R/scals.R')
source('R/activs.R')
source('R/optim.R')
source('R/stopCriteria.R')
source('R/config.R')


# loadModule("evaluator",what=T)

# IBHM method -------------------------------------------------------------

TrainIBHM <- function(x,y, config = ConfigureIBHM()  ){
  if(config$jit){    
    require(compiler)
    setCompilerOptions(suppressAll = TRUE)
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


DoIteration <- function(prevCtx){
  ctx <- prevCtx
  ctx$log('\nIteration ',ctx$iteration, ' RMSE: ', CalcRMSE(ctx))  
  ctx$iteration <- ctx$iteration + 1
  
  ctx <- FindScal(ctx)
  if(ctx$continue){
    ctx <- FindActiv(ctx) 
    ctx$m$par$w <- append(ctx$m$par$w,1)  
    ctx <- OptimizeWeights(ctx)
    UpdateResidual(ctx)
  }else{
    ctx$log('Stopping')
    prevCtx$continue <- F
    prevCtx
  }
}

CalcRMSE <- function(ctx){
  sqrt(mean((ctx$y-predict.IBHM(ctx$m,ctx$x))^2))
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
  params$scal$optim.params$par.min <- min(-100,-x.m)
  params$scal$optim.params$par.max <- max(100, x.m)
  
  params$activ$optim.params$par.min <- min(-100,-x.m)
  params$activ$optim.params$par.max <- max(100,x.m)
   
  sc.mask <- sample(nrow(x),ceiling(nrow(x)*params$sc.frac))

  append(params,
         list( x = as.matrix(x[-sc.mask,]),
               y = as.matrix(y[-sc.mask]), 
               yr = y,               
               x.final = x,
               y.final = y, 
               m=CreateEmptyModel(as.matrix(x[-sc.mask,]),as.matrix(y[-sc.mask]), train.config = params$unparsed.config, filtering.function = params$wf), 
               scal.y = list(),
               continue=TRUE, 
               iteration=0,
               log = log,
               sc = ValidationSC(as.matrix(x[sc.mask,]),as.matrix(y[sc.mask]), params$sc.max.failures, params$sc.approximate.timeout),
               train.start = lubridate::now()
         )
  )
}


# IBHM model structure ----------------------------------------------------
CreateEmptyModel <- function(x=NULL,y=NULL, train.config = NULL, filtering.function){
  m <- list(par = list(
                w0=0,
                w=vector(),
                d=list(),                            
                a=vector(),
                b=vector()),
            scals=list(),            
            activs=list(),            
            train=list(x=x,y=y, filtering.function = filtering.function, w.pars = vector(), config = train.config))
  class(m) <- 'IBHM'
  return(m)
}



# Standard S3 methods -----------------------------------------------------
predict.IBHM <- function(object,x = NULL,...){  
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


length.IBHM <- function(x){
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

summary.IBHM <- function(object,...){  
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
  class(res) <- 'summary.IBHM'
  res
}

print.summary.IBHM <- function(x,...){
  cat('Model equation: ', x$model,'\n',
      'Model size: ',x$model.size,'\n\n',
      'Train set dim: ', x$TrainDim, ' Train set size: ', x$TrainSize,'\n',      
      'MSE:  ', x$mse, ' Std. dev:', x$se.sd,'\n',
      'RMSE: ', x$rmse, '\n',
      'Pearson correlation coefficient: ', x$cor,'\n')
}



regularize <- function(params){
  #max(sqrt(sum(params^2))-1,0)
  sum(abs(params))
}

# Finding a scalarization function ----------------------------------------

FindScal <- function(ctx){
  within(ctx,{
    log(' Finding next scal')
                
    best <- list(scal=NULL, eval = Inf, d = NULL)
    
    for( s in  scal$candidates){
      candidate <- OptimizeScal(s,ctx)
      log('  ',expr(s), ' eval : ', candidate$eval)
      
      if(candidate$eval < best$eval){
        best <- candidate        
      }
    }
    if(is.finite(best$eval)){
      log(' Best scal: ',expr(best$scal), ' d: ', best$d,' w.par:',best$w.par)
        
      m$scals[[iteration]]  <- best$scal
      m$par$d[[iteration]] <- best$d
      m$train$w.pars[[iteration]] <- best$w.par
    
      zk <- best$scal(x, best$d)
      w <- wf(zk, best$w.par)   
      
    }else{
      log(' Could not find next scal - stopping.')
      continue <- F
    }
  })
}


OptimizeScal <- function(scal, ctx){

  goal <- function(par, lambda){
    zCandidate <- scal(ctx$x, par)
    w <- ctx$wf(zCandidate, ctx$weighting.par)

    cor.y <- weightedR(ctx$yr, zCandidate,w)

    if(is.nan(cor.y)){
      cor.y<-0
    }

    return(1 - abs(cor.y)  + lambda*regularize(par))
  }

  n <- ScalParamDim(scal, ctx$x)
  res <- ctx$scal$optim(function(par) goal(par,ctx$scal$lambda),n,ctx$scal$optim.params)
    
  list(scal = scal, eval = res$eval, w.par=ctx$weighting.par, d=res$par)
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
  goal <- function(par, lambda){          
    a <- par[[1]]
    b <- par[[2]]
    vk <- activ(a*zk+b)    
    val<-weightedR(ctx$yr, vk, ctx$w)  
    
    if(is.nan(val) || !is.finite(val)){
      val<-0
    }
    
    eval <- 1-abs(val) + lambda*regularize(par)     
    
    if(is.nan(eval)){  
      eval <- Inf        
    } 
    return(eval)
  }  
  
  res <- ctx$activ$optim(function(par) goal(par, ctx$scal$lambda), 2, ctx$activ$optim.params)  
      
  ctx$log('  ',expr(activ),' eval:',res$eval)
  list(activ=activ, eval=res$eval,  a=res$par[[1]], b=res$par[[2]], v = activ(res$par[[1]]*zk+res$par[[2]]))
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
      
      return(mean((y - predict.IBHM(m, x))^2))
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
      return(mean((y - predict.IBHM(m,x))^2))
    }
              
    res <- optim(unlist(m$par),goal, method="BFGS", control=list(maxit=maxit, trace=F))  
    m$par <- relist(res$par, m$par)          
  })  
}

UpdateResidual <- function(ctx){
  ctx$yr <- ctx$y - predict.IBHM(ctx$m, ctx$x)
  ctx
}





