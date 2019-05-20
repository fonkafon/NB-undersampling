source("setup_edit.R")    #setwd, load all files into dfAll
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('searchKnn_UnderS.R') #uncomment this line for NB-Basic
#source('searchKnn_UnderS_Tomek.R') #uncomment this line for NB-Tomek
#source('searchKnn_UnderS_Common.R') #uncomment this line for NB-Comm
#source('searchKnn_UnderS_Recursive.R') #uncomment this line for NB-Rec
library(ggplot2)
library(DMwR)
source('makeData_imb2_10042019.R')
library(ggplot2)

seed <- 1987

#vary imb ratio
endJ <- 6
imbalance <- c(1.5,3,12,30,60,120)  
for (j in 1:endJ){
  imb <- imbalance[j]
  #run over 0-100% overlap (101 runs)
  endI <- 11 
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
    
    P <- nrow(df[df$label == 'positive',])
    N <- nrow(df[df$label == 'negative',])
    imb <- N/P
    
    k<-sqrt(nrow(train))+sqrt(imb)
    train <- searchKnn(train,k)
    
    tmpN <- train[(train$probN==1)&(train$label=='negative'),] 
    tmpP <- train[train$label=='positive',]
    train <- rbind(tmpP,tmpN)
    
    train$probP <- NULL
    train$probN <- NULL
   
    #fit model after undersampling
    resultUnder <- fitResult(train,test)
    resultUnder <- rbind(data = i,resultUnder,imb, samples= nrow(train),
                         maj=maj, min = min)
    ifelse(i == 1, allResultUnder <- resultUnder,
           allResultUnder <- cbind(allResultUnder, resultUnder))  
    cat(paste0('\n',i,' finished','\n')) 
  }
  cat(paste0('\n',j,' finished','\n'))
  writeAppend('knn_sim_Basic_sqrtImb+sqrtN_16Apr19_svmRadial',t(allResultUnder),1)
}  
