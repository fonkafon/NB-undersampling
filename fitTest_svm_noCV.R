#fit model and test 
library(caret) #rf
#library(e1071) #svm
source("datPartition.R")
source("trainmodel2_noCV.R")
source("perfResults2.R")
source("calAuc_noCV.R")

fitTest <- function(train, test){
  fit <- trainmodel(train, 'svmRadial') 
  #fit <- trainmodel(train, 'rf') 
  result <- perfResults2(fit, test)
  return(result)
}

