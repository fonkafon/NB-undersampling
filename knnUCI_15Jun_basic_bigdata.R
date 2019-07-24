source("setup_edit.R") 
df <- read.table("C:/kddcup/2008/Features.txt", sep ="")
label <- read.table("C:/kddcup/2008/Info.txt", sep ="")
tmpdf <- df
#df<-tmpdf
df <- cbind(df, factor(label[,1])) #, levels = c("negative", "positive")))
names(df)[ncol(df)] <- 'label'

df <- read.table("dfKDD2008.csv", sep ="")

##MNIST
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

#tmp <- df
#tmp$V2 <- as.numeric(tmp$V2)
#tmp$V3 <- as.numeric(tmp$V3)
#tmp$V4 <- as.numeric(tmp$V4)
#names(tmp)[42] <- "label"
#df <- tmp

#source("fitTest.R")
#source("fitTest_svm_parallel.R")
source("fitTest_svm_noCV.R")
#source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
source("loadfile_edit.R")
#source('searchKnn_UnderS_parallel.R')
source('searchKnn_UnderS.R')
library(ggplot2)
library(DMwR)

#run through all datasets
set.seed(seed)
#trim data down to 50K 
#dPart <- datPartition(df, 0.1)
#df <- dPart[[1]] 

#start of training
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
  
  library(doParallel)
  no_cores <- detectCores()
  cl <- makePSOCKcluster(no_cores)
  registerDoParallel(cl)
  
  system.time(resultOrg <- fitResult(train,test))
  #stopCluster(cl)
  registerDoSEQ()
  write.csv(resultOrg, "kdd2008original.csv")
  
  #resultOrg <- rbind(data = i,resultOrg,imb, samples =nrow(train))
  #ifelse(i == 1, allResultOrg <- resultOrg,
  #       allResultOrg <- cbind(allResultOrg, resultOrg))
  
  k<-(sqrt(imb))+sqrt(nrow(train))
  #k<-0.1*(nrow(train))
  #k<-sqrt(imb*nrow(train))
  cl <- makePSOCKcluster(no_cores-1)
  registerDoParallel(cl)
  trainKeep <- searchKnn(train,k)
  stopCluster(cl)
  registerDoSEQ()
  
  trainOrg <- trainKeep
  train <- trainKeep
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
  resultSMOTE <- fitResult(trainSMOTE,test, ovPercent=0)
  resultSMOTE <- rbind(data = i,resultSMOTE,imb, samples= nrow(trainSMOTE),
                       maj=nrow(trainSMOTE[trainSMOTE$label=='negative',]), 
                       min = nrow(trainSMOTE[trainSMOTE$label=='positive',]))
  ifelse(i == 1, allResultSMOTE <- resultSMOTE,
         allResultSMOTE <- cbind(allResultSMOTE, resultSMOTE))
  
  cat(paste0(i,' finished','\n')) 


#plot original & undersampled training sets
#savePlots(imb, endI, trainAll, trainUnderAll,trainSMOTEAll)

#writeAppend('orgPerf_UCI_sqrN_14Jun',t(allResultOrg),1)
writeAppend('knnPerf_UCI_sqrtIMBxN_17Jun',t(allResultUnder),1)
writeAppend('knnSMOTEPerf_UCI_sqrtIMBxN_17Jun',t(allResultSMOTE),1)
#writeAppend('undersampledTrain_UCI',trainUnderAll,1)
#writeAppend('train_UCI',trainAll,1)
