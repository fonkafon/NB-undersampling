source("fitTest.R")
source('writeAppend.R')
source('makeData_imb2_10042019.R')
source('famCurve_imb_facet.R')
source('searchKnn.R')
source('fitResult_imb.R')
library(ggplot2)

seed <- 1987

#vary imb ratio
endJ <- 6
imbalance <- c(1.5,3,12,30,60,120)  
for (j in 1:endJ){
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
    
    result <- fitResult(train,test,ovPercent)
    ifelse(i == 1, allResult <- result,
           allResult <- cbind(allResult, result))    
  }
  allResult <- as.data.frame(t(allResult))
  allResult <- cbind(allResult, imb)
  ifelse(j ==1, allResult_new <- allResult, allResult_new <- rbind(allResult_new,allResult))
  cat('imb=',imb,'ov=',ovPercent,'\n')
}
#save fam plots to 1 pdf file
#fName <- paste0('perfVsOv', filenum, '.pdf')
#pdf(fName)
#for(n in famPlot){replayPlot(n)}
#graphics.off()

#p<-famCurve(allResult_new,imb)
#print(p)

filenum <- 12042019
writeAppend('svmRadial_6000constantMajor', allResult_new, filenum)
