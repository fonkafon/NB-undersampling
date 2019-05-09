source("fitTest.R")
source('writeAppend.R')
#source('makeData_imb2_facet_13112018_constantMinor.R')
#source('makeData_imb2_10042019_constantMinor.R')
source('makeData_imb2_10042019.R')
source('famCurve_imb_facet.R')
source('searchKnn.R')
source('fitResult_imb.R')
#source('ovError.R')
#source('ovEstimate.R')
#source('minimize_imb.R') #find optimal N 
#source('ovError.R')
library(ggplot2)
#library(unbalanced)
#library(C50)

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
    dPart <- datPartition(df, 0.7)
    train <- dPart[[1]]
    test <- dPart[[2]]
    
    #resampling
    #df <- ubBalance(X, Y, type="ubSMOTE", positive=1, percOver=200, percUnder=200,
    #                k=5, perc=50, method="percPos", w=NULL, verbose=FALSE)
    #perc = percentage of positive after undersampling
    #levels(train$label)<- c(0,1)  #ubBalance only works with class 0 and 1
    #trainUnder <- ubBalance(train[,-ncol(train)], train[,ncol(train)], type="ubUnder", 
    #                  positive= 'positive', perc=50, method="percPos", w=NULL, verbose=FALSE)
    #train <- data.frame(cbind(data.frame(trainUnder$X), label = trainUnder$Y))
    #levels(train$label)<- c('positive','negative') #change labels back
    
    # train$label2 <- 0 
    # train[train$label == 'negative',]$label2 <- 'positive'
    # train[train$label == 'positive',]$label2 <- 'negative'
    # train$label2 <- factor(train$label2, levels= c('positive','negative'))
    # train <- train[,-(ncol(train)-1)]
    # names(train)[ncol(train)]<- 'label'
    
    result <- fitResult(train,test,ovPercent)
    ifelse(i == 1, allResult <- result,
           allResult <- cbind(allResult, result)) 
    
  }
  
  #write & plot overall results vs overlap
  #filenum <- 1  
  #writeAppend('OVvsPerf',allResult, filenum)
  allResult <- as.data.frame(t(allResult))
  allResult <- cbind(allResult, imb)
  ifelse(j ==1, allResult_new <- allResult, allResult_new <- rbind(allResult_new,allResult))
  cat('imb=',imb,'ov=',ovPercent,'\n')
  #store fame plot in a vector
}
#save fam plots to 1 pdf file
#fName <- paste0('perfVsOv', filenum, '.pdf')
#pdf(fName)
#for(n in famPlot){replayPlot(n)}
#graphics.off()

#p<-famCurve(allResult_new,imb)
#print(p)

#write.csv(allResult_new, 'allresult.csv')
filenum <- 12042019
writeAppend('svmRadial_6000constantMajor', allResult_new, filenum)
#writeAppend('threshold',t(allThreshold), filenum) 
#writeAppend('error_imb',t(allError), filenum) 


