source("setup.R") 
endI <-44
dfAll <- setup('Data/C', endI)
fileName <- 'C'

source("fitTest.R")
source("fitResult_imb.R")
source('writeAppend.R')
source('makeData_TestEstimator_v2.R')
source('famCurve.R')
source('searchKnn_UnderS.R')
source("loadIndFile.R")
source('ovEstimate.R')
source('savePlotsUCI.R') ###
library(ggplot2)
library(DMwR)

#run through all datasets 
for (i in 1:endI){
  #if(i==18) next
  df <- loadIndFile(dfAll, i)
  dPart <- datPartition(df, 0.8)
  train <- dPart[[1]]
  test <- dPart[[2]]
  
  #estimate ov
  P <- nrow(df[df$label == 'positive',])
  N <- nrow(df[df$label == 'negative',])
  #imb <- P/N*100
  imb <- N/P
  #resultOrg <- fitResult(train,test, ovPercent=0)
  #resultOrg <- rbind(data = i,resultOrg,imb, samples =nrow(train))
 # ifelse(i == 1, allResultOrg <- resultOrg,
  #       allResultOrg <- cbind(allResultOrg, resultOrg))
  
  #k<-(sqrt(imb))+sqrt(nrow(train))
  #k<-0.1*(nrow(train))
  k<-sqrt(imb*nrow(train))
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
  set.seed(seed)
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
  resultUnder <- fitResult(train,test, ovPercent=0)
  resultUnder <- rbind(data = i,resultUnder,imb, samples= nrow(train),
                       maj=maj, min = min)
  ifelse(i == 1, allResultUnder <- resultUnder,
         allResultUnder <- cbind(allResultUnder, resultUnder))
  
  #fit RF after SMOTE
  resultSMOTE <- fitResult(trainSMOTE,test, ovPercent=0)
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
writeAppend('knnPerf_UCI_sqrtIMBxN_17Jun',t(allResultUnder),1)
writeAppend('knnSMOTEPerf_UCI_sqrtIMBxN_17Jun',t(allResultSMOTE),1)
#writeAppend('undersampledTrain_UCI',trainUnderAll,1)
#writeAppend('train_UCI',trainAll,1)
