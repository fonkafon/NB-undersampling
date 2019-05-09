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
#source('ovEstimate.R')
#source('savePlotsUCI.R') ###
library(ggplot2)
library(DMwR)
source('makeData_imb2_10042019.R')
#source('famCurve_imb_facet.R')
library(ggplot2)

#seed <- 540 for imb1.5+ov100
seed <- 1987

#vary imb ratio
endJ <- 5
imbalance <- c(1.5,3,12,30,60,120)  
for (j in 1:endJ){
  #imb <- 0.1*j
  imb <- imbalance[j]
  #run over 0-100% overlap (101 runs)
  endI <- 11 #11
  for (i in 11:endI){
    ovPercent <- (i-1)*10
    df <- makeData(ovPercent, imb, endI)
    k <- length(df)
    
    df[,-k] <- scale(as.numeric(unlist(df[,-k])))
    
    #partition - testing/training
    set.seed(seed)
    dPart <- datPartition(df, 0.7)
    train <- dPart[[1]]
    test <- dPart[[2]]
    
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
    trainSMOTE <-  bimba::SMOTE(train, perc_min = 50, k = 3)
    
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
    
    cat(paste0('\n',i,' finished','\n')) 
  }

  #plot original & undersampled training sets
  #savePlots(imb, endI, trainAll, trainUnderAll,trainSMOTEAll)
  
  writeAppend('orgPerf_sim_16Apr19_svmRadial',t(allResultOrg),2)
  writeAppend('knn_sim_Basic_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultUnder),2)
  writeAppend('knnSMOTE_sim_Basic_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultSMOTE),2)
}  


