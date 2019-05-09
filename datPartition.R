#Prepare and partition data - training & testing
#library(caret)
datPartition <- function(dat, pTrain){ # pTrain = % of training (0-1)
  set.seed(1987)
  trainIndex <- createDataPartition(dat[,length(dat)],p = pTrain, 
                                    list = FALSE, times = 1)  #preserve overall class distribution
  train <- dat[trainIndex,, drop = FALSE]
  test <- dat[-trainIndex,, drop = FALSE]
  list <- list(train, test)
  return(list)
}