source("setup_edit.R")    #setwd, load all files into dfAll
#setup() 
source("fitTest.R")
source("fitResult_imb.R")
source('writeAppend.R')
source("loadfile_edit.R")

library(unbalanced)
library(bimba)

algor <- c('rf','svmRadial') #'mlp', , 'svmLinear'
#algor <- c('J48', 'rf', 'mlp', 'svmRadial', 'svmLinear')
numDat <- 44
numAlg <- 1
ovPercent<-0

#run through all datasets 
for (i in 1:numDat){
  #if(i == 22) next
  if(i ==33 | i == 22) next
  for (j in 1:numAlg){
    df <- loadIndFile('Data/C', i)
    #load individual file
    
    dPart <- datPartition(df, 0.8)
    train <- dPart[[1]]
    test <- dPart[[2]]
    P <- nrow(train[train$label == 'positive',])
    N <- nrow(train[train$label == 'negative',])
    imb <- N/P
    
    print(table(train$label))
    
    train$label <- factor(train$label, levels = c("negative", "positive"), labels = c("0", "1"))
    
    #k<-sqrt(nrow(train))+sqrt(imb)
    trainENN <- ubENN(train[,-ncol(train)], train$label, k = 3, verbose = FALSE)
    train <- cbind(trainENN$X, label = trainENN$Y)
    
    train$label <- factor(train$label, levels = c("0", "1"), labels = c("negative", "positive"))
    
    train <-  bimba::SMOTE(train, perc_min = 50, k = 3)
    
    print(table(train$label))
 
    result <- fitResult(train,test)

    ifelse(i==1, allResult <- result, allResult <- cbind(allResult,result))
  }
  cat(paste0(i,' finished','\n')) 
}
write.csv(t(allResult), file = 'UCI_ENN+SMT_14Dec18_k=3.csv')
#write.csv(t(allResult), file = 'UCI_Smote_35_RUS50_k=3_m=2.csv')
