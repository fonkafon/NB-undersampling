#collect selected performance resutls from each dataset
library (base)
perfResults2 <- function(fit, test){
  set.seed(seed)
  results <- predict(fit, test)              
  results2 <- predict(fit, test, type = 'prob')[,2]
  #results2 <- predict(fit, test, type = 'prob')
  auc <- calAuc(results2, test)
  #msg <- paste0('AUC of C', i, ' = ')
  #cat(msg, unlist(auc@y.values),"\n")
  cf <- confusionMatrix(results, test$label, positive = "positive")
  cf <- cf$byClass
  cf <- as.matrix(c(cf[1], cf[2], cf[7], cf[11], unlist(auc@y.values)))
  #colnames(cf) <- as.matrix(paste0('OV ', percent, '%'  )) 
  colnames(cf) <- paste0('data',i)
  rownames(cf)[5] <- 'AUC'  
return(cf)
}


