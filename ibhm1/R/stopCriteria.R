
# Stop criteria -----------------------------------------------------------


IterationSC1 <- function(max.iterations){
  list( 
    eval = function(ctx){
      ctx$continue <- ctx$iteration < ctx$sc$max.iterations
      ctx
    },        
    max.iterations = max.iterations
  )    
}


ValidationSC1 <- function(x,y){
  list( 
    eval = function(ctx){              
      yh <- predict(ctx$m,ctx$sc$x)
      val <- sqrt(mean((yh-ctx$sc$y)^2))
      ctx$log(' Validation RMSE: ',val)
            
      if(val > ctx$sc$last.eval){
        ctx$continue <- FALSE
        ctx$m <- ctx$sc$best.model
        
      }else{
        ctx$sc$last.eval <- val
        ctx$sc$best.model <- ctx$m
      }
      
      ctx      
    },        
    x = x,
    y = y,    
    last.eval = Inf,
    best.model = list()
  )
}