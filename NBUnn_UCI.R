source("setup_edit.R")    #setwd, load all files into dfAll
source("fitTest_svm.R")
source("fitResult_imb.R")
source('writeAppend.R')
#source('searchKnn_UnderS.R') #uncomment this line for NB-Basic
#source('searchKnn_UnderS_Tomek.R') #uncomment this line for NB-Tomek
#source('searchKnn_UnderS_Common.R') #uncomment this line for NB-Comm
#source('searchKnn_UnderS_Recursive.R') #uncomment this line for NB-Rec
source("loadfile_edit.R")
library(ggplot2)
#library(DMwR)

numDat <- 24
ovPercent<-0

#run through all datasets 
for (i in 1:numDat){
  j <- c(1,4,5,6,7,8,9,14,15,16,20,21,23,24,25,28,29,32,33,36,39,41,42,44)
  df <- loadIndFile('Data/C', j[i])
  set.seed(seed)
  dPart <- datPartition(df, 0.8)
  train <- dPart[[1]]
  test <- dPart[[2]]
  
  #estimate ov
  P <- nrow(df[df$label == 'positive',])
  N <- nrow(df[df$label == 'negative',])
  imb <- N/P

  train <- searchKnn(train) #undersampled training set
  
  #SMOTE after kNN undersampling
  maj <- nrow(train[train$label == 'negative',])
  min <- nrow(train[train$label == 'positive',])
  
  #fit model after undersampling
  resultUnder <- fitResult(train,test)
  resultUnder <- rbind(data = i,resultUnder,imb, samples= nrow(train),
                       maj=maj, min = min)
  ifelse(i == 1, allResultUnder <- resultUnder,
         allResultUnder <- cbind(allResultUnder, resultUnder))
  
  cat(paste0(i,' finished','\n')) 
}
writeAppend('knn_UCI_Tomek_sqrtImb+sqrtN_15Apr19_svmRadial',t(allResultUnder),1)
