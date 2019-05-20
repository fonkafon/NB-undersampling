#fit model and test 
library(caret) #rf
#library(e1071) #svm
source("datPartition.R")
source("trainmodel2.R")
source("perfResults2.R")
source("calAuc.R")

fitTest <- function(train, test){
  #fit <- trainmodel(train, 'svmRadial') 
  fit <- trainmodel(train, 'rf') 
  result <- perfResults2(fit, test)
  return(result)
}

