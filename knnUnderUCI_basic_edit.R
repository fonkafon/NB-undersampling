source("setup_edit.R")    #setwd, load all files into dfAll
#setup() 
#source("fitTest.R")
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('makeData_TestEstimator_v2.R')
#source('famCurve.R')
source('searchKnn_UnderS.R')
#source('searchKnn_UnderS_Tomek.R')
#source('searchKnn_UnderS_Common.R')
#source('searchKnn_UnderS_Recursive.R')
source("loadfile_edit.R")
#source('ovEstimate.R')
#source('savePlotsUCI.R') ###
library(ggplot2)
library(DMwR)

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
  set.seed(seed)
  dPart <- datPartition(df, 0.8)
  train <- dPart[[1]]
  test <- dPart[[2]]
  
  #estimate ov
  P <- nrow(df[df$label == 'positive',])
  N <- nrow(df[df$label == 'negative',])
  imb <- N/P

  resultOrg <- fitResult(train,test)
  resultOrg <- rbind(data = i,resultOrg,imb, samples =nrow(train))
  ifelse(i == 1, allResultOrg <- resultOrg,
         allResultOrg <- cbind(allResultOrg, resultOrg))
  
  k<-sqrt(nrow(train))+sqrt(imb)
  train <- searchKnn(train,k)
  trainOrg <- train
  tmpN <- train[(train$probN==1)&(train$label=='negative'),] 
  tmpP <- train[train$label=='positive',]
  train <- rbind(tmpP,tmpN)
  
  train$probP <- NULL
  train$probN <- NULL
  trainOrg$probP <- NULL
  trainOrg$probN <- NULL
  
  #SMOTE after kNN undersampling
  maj <- nrow(tmpN)
  min <- nrow(tmpP)
  #over <- (maj/min-1)*100  #makes #min = #maj  (round up)
  #under <- (maj/(maj-min))*100 #makes #maj ~ the same 
  #trainSMOTE<-SMOTE(label~.,train,perc.over = over, perc.under= under) 
  #library(ROSE)
  #trainSMOTE <- ROSE(label~.,train, N=maj*2, p=0.5, seed = 123)$data
  
  #devtools::install_github("RomeroBarata/bimba")
  library(bimba)
  trainSMOTE <-  SMOTE(train, perc_min = 50, k = 3)
  
  #prepare data to save 
  #trainOrg <- cbind(data=i, size=nrow(trainOrg), imb, trainOrg)
  #trainUnder <- cbind(data=i, size=nrow(train), imb, train)
  #ifelse(i == 1, trainUnderAll <- trainUnder,
  #       trainUnderAll <- rbind(trainUnderAll, trainUnder))
  #ifelse(i == 1, trainAll <- trainOrg,
   #      trainAll <- rbind(trainAll, trainOrg))
  
  #trainUndSMOTE <- cbind(data=i, size=nrow(trainSMOTE), imb, trainSMOTE)
  #ifelse(i == 1, trainSMOTEAll <- trainUndSMOTE,
  #       trainSMOTEAll <- rbind(trainSMOTEAll, trainUndSMOTE))
  
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

writeAppend('orgPerf_UCI_15Apr19_svmRadial',t(allResultOrg),2)
writeAppend('knn_UCI_Basic_sqrtImb+sqrtN_15Apr19_svmRadial',t(allResultUnder),2)
writeAppend('knnSMOTE_UCI_Basic_sqrtImb+sqrtN_15Apr19_svmRadial',t(allResultSMOTE),2)
#writeAppend('undersampledTrain_UCI',trainUnderAll,1)
#writeAppend('train_UCI',trainAll,1)
