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
numDat <- 24
numAlg <- 2
ovPercent<-0

#run through all datasets 
for (i in 1:numDat){
  j <- c(1,4,5,6,7,8,9,14,15,16,20,21,23,24,25,28,29,32,33,36,39,41,42,44)
  #if(i==18) next
  df <- loadIndFile('Data/C', j[i])
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
    print(table(train$label))
    
    train$label <- factor(train$label, levels = c("0", "1"), labels = c("negative", "positive"))
    
    P2 <- nrow(train[train$label == 'positive',])
    N2 <- nrow(train[train$label == 'negative',])
    #if maj > min, SMOTE to get minor = 50% of total instance
    #if min > maj, SMOTE to get extra 50% minor (total minor = 1.5 x original minor)
    ifelse(P2 < N2, train <-  bimba::SMOTE(train, perc_min = 50, k = 3),
           train <-  bimba::SMOTE(train, perc_over = 50, k = 3)) 
    
    print(table(train$label))
 
    result <- fitResult(train,test)

    ifelse(i==1, allResult <- result, allResult <- cbind(allResult,result))
  cat(paste0(i,' finished','\n')) 
}
write.csv(t(allResult), file = 'UCI_ENN+SMT_15Apr19_svmRadial_k=3.csv')
#write.csv(t(allResult), file = 'UCI_Smote_35_RUS50_k=3_m=2.csv')
