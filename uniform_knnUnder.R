source("setup_edit.R")    #setwd, load all files into dfAll
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
source('searchKnn_UnderS.R')
#source('searchKnn_UnderS_Tomek.R')
#source('searchKnn_UnderS_Common.R')
#source('searchKnn_UnderS_Recursive.R')
library(ggplot2)
library(DMwR)
source('makeData_imb2_10042019.R')
library(ggplot2)

seed <- 1987

#vary imb ratio
endJ <- 5
imbalance <- c(1.5,3,12,30,60,120)  
for (j in 1:endJ){
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
   
    #fit RF after undersampling
    resultUnder <- fitResult(train,test)
    resultUnder <- rbind(data = i,resultUnder,imb, samples= nrow(train),
                         maj=maj, min = min)
    ifelse(i == 1, allResultUnder <- resultUnder,
           allResultUnder <- cbind(allResultUnder, resultUnder))  
    cat(paste0('\n',i,' finished','\n')) 
  }
 
  writeAppend('orgPerf_sim_16Apr19_svmRadial',t(allResultOrg),1)
  writeAppend('knn_sim_Basic_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultUnder),1)
}  
