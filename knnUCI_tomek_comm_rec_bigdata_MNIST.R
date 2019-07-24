source("setup_edit.R")    #setwd, load all files into dfAll
df <- read.table("C:/MNIST/mnist_train.txt", sep =",")
df<- df[-1,] #header
df[] <- lapply(df, as.character)
df[] <- lapply(df, as.numeric)
df <- cbind(df[,-1],df[,1])
names(df)[ncol(df)] <- 'label'
dftmp <- df

#combine the other classes besides 5 
df$label <- ifelse(df$label == 5, 'positive', 'negative')
df$label <- as.factor(df$label)

#undersampling class 5
set.seed(seed)
df <- df[-sample(which(df$label=="positive"), 2710),]

#setup() 
source("fitTest_svm_noCV.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('searchKnn_UnderS.R')
#source('searchKnn_UnderS_Tomek.R')
#source('searchKnn_UnderS_Common.R')
source('searchKnn_UnderS_Recursive.R')
source("loadfile_edit.R")
library(ggplot2)
#library(DMwR)

levels(df$label) <- list('negative'= -1, 'positive' = 1)
dPart <- datPartition(df, 0.8)
train <- dPart[[1]]
test <- dPart[[2]]
levels(test$label) <- list('negative'= -1, 'positive' = 1)
levels(train$label) <- list('negative'= -1, 'positive' = 1)
  
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
  #library(bimba) #perc_min = 50 balances the set
  #set.seed(seed)
  #trainSMOTE <-  SMOTE(train, perc_min = 50, k = 3)
  
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
