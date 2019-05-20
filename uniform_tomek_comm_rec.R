source("setup_edit.R")
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('makeData_TestEstimator_v2.R')
#source('famCurve.R')
#source('searchKnn_UnderS.R')
#source('searchKnn_UnderS_Tomek.R')
#source('searchKnn_UnderS_Common.R')
source('searchKnn_UnderS_Recursive.R')
#source('ovEstimate.R')
#source('savePlotsUCI.R') ###
library(ggplot2)
library(DMwR)
source('makeData_imb2_10042019.R')
#source('famCurve_imb_facet.R')
library(ggplot2)

seed <- 1987

#vary imb ratio
endJ <- 6
imbalance <- c(1.5,3,12,30,60,120)  
for (j in 1:endJ){
  #imb <- 0.1*j
  imb <- imbalance[j]
  #run over 0-100% overlap (101 runs)
  endI <- 11 #11
  for (i in 1:endI){
    ovPercent <- (i-1)*10
    df <- makeData(ovPercent, imb, endI)
    k <- length(df)
    
    df[,-k] <- scale(as.numeric(unlist(df[,-k])))
    
    #partition - testing/training
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
  
  cat(paste0('\n',i,' finished','\n')) 
  }
  cat(paste0('\n',j,' finished','\n'))

#plot original & undersampled training sets
#savePlots(imb, endI, trainAll, trainUnderAll,trainSMOTEAll)

#writeAppend('orgPerf_UCI_sqrN_14Jun',t(allResultOrg),1)
writeAppend('knn_sim_Rec_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultUnder),1)
writeAppend('knnSMOTE_sim_Rec_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultSMOTE),1)
#writeAppend('undersampledTrain_UCI',trainUnderAll,1)
#writeAppend('train_UCI',trainAll,1)
}
