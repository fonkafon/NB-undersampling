source("setup_edit.R")    #setwd, load all files into dfAll
#setup() 
#source("fitTest.R")
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('makeData_TestEstimator_v2.R')
#source('famCurve.R')
#source('searchKnn_UnderS.R')
source('searchKnn_UnderS_Tomek.R')
#source('searchKnn_UnderS_Common.R')
#source('searchKnn_UnderS_Recursive.R')
source("loadfile_edit.R")
#source('ovEstimate.R')
#source('savePlotsUCI.R') ###
library(ggplot2)
#library(DMwR)

#algor <- c('rf','svmRadial') #'mlp', , 'svmLinear'
#algor <- c('J48', 'rf', 'mlp', 'svmRadial', 'svmLinear')
numDat <- 24
#numAlg <- 2
ovPercent<-0

#run through all datasets 
for (i in 1:numDat){
  j <- c(1,4,5,6,7,8,9,14,15,16,20,21,23,24,25,28,29,32,33,36,39,41,42,44)
  #if(i==18) next
  df <- loadIndFile('Data/C', j[i])
  set.seed(seed)
  dPart <- datPartition(df, 0.8)
  train <- dPart[[1]]
  test <- dPart[[2]]
  
  #estimate ov
  P <- nrow(df[df$label == 'positive',])
  N <- nrow(df[df$label == 'negative',])
  imb <- N/P
  
  #resultOrg <- fitResult(train,test, ovPercent=0)
  #resultOrg <- rbind(data = i,resultOrg,imb, samples =nrow(train))
  #ifelse(i == 1, allResultOrg <- resultOrg,
  #       allResultOrg <- cbind(allResultOrg, resultOrg))
  
  #k<-sqrt(nrow(train))
  #k<-2
  train <- searchKnn(train) #undersampled training set
  
  #SMOTE after kNN undersampling
  maj <- nrow(train[train$label == 'negative',])
  min <- nrow(train[train$label == 'positive',])
  
  #devtools::install_github("RomeroBarata/bimba")
  library(bimba) #perc_min = 50 balances the set
  set.seed(seed)
  trainSMOTE <-  SMOTE(train, perc_min = 50, k = 3)
  
  #fit RF after undersampling
  resultUnder <- fitResult(train,test)
  resultUnder <- rbind(data = i,resultUnder,imb, samples= nrow(train),
                       maj=maj, min = min)
  ifelse(i == 1, allResultUnder <- resultUnder,
         allResultUnder <- cbind(allResultUnder, resultUnder))
  
  #fit RF after SMOTE
  resultSMOTE <- fitResult(trainSMOTE,test)
  resultSMOTE <- rbind(data = i,resultSMOTE,imb, samples= nrow(trainSMOTE),
                       maj=nrow(trainSMOTE[trainSMOTE$label=='negative',]), 
                       min = nrow(trainSMOTE[trainSMOTE$label=='positive',]))
  ifelse(i == 1, allResultSMOTE <- resultSMOTE,
         allResultSMOTE <- cbind(allResultSMOTE, resultSMOTE))
  
  cat(paste0(i,' finished','\n')) 
}

#plot original & undersampled training sets
#savePlots(imb, endI, trainAll, trainUnderAll,trainSMOTEAll)

#writeAppend('orgPerf_UCI_sqrN_14Jun',t(allResultOrg),1)
writeAppend('knn_UCI_Tomek_sqrtImb+sqrtN_15Apr19_svmRadial',t(allResultUnder),1)
writeAppend('knnSMOTE_UCI_Tomek_sqrtImb+sqrtN_15Apr19_svmRadial',t(allResultSMOTE),1)
#writeAppend('undersampledTrain_UCI',trainUnderAll,1)
#writeAppend('train_UCI',trainAll,1)
