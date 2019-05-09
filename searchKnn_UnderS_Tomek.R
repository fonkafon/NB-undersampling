#search knn 
library(FNN)
searchKnn <- function(df){
  #k <- sqrt(nrow(df))
  k<-(sqrt(imb))+sqrt(nrow(train))
  #sort positive and negative instances
  dfP <- df[df$label=='positive',]
  dfN <- df[df$label=='negative',]
  df <- rbind(dfN, dfP)
  numN <- nrow(dfN) #last row of the negative class
  
  set.seed(seed)
  nn <- get.knn(df[,-ncol(df)],k)   
  nnIndex <- as.data.frame(nn$nn.index)
  
  for (n in 1: numN){ #check NNs of negative only
    count <- 0
    for (m in 1:dim(nnIndex)[2]){ #dim(nnIndex)[2]==k 
      if(nnIndex[n,m] <= numN){#nn is negative 
        count <- count+1
      } else {#nn is negative 
        checkNNofposNN <- (nnIndex[nnIndex[n,m],] == n) #T, F
        tomek <- sum(checkNNofposNN)
        if(tomek < 1) {
          count <- count+1 #no link found
        } else {break} #link found, skip to next instance
      }
    }  
    if(count == dim(nnIndex)[2]){
      ifelse(exists('trainN'), trainN <- rbind(trainN, df[n,]),
             trainN <- df[n,])
    }
  }
  undTrain<-rbind(trainN, dfP)
 
  #negInstance <- nnIndex[(numP+1):nrow(df),] #List negative instances with NN only
  #tmp <- as.matrix(negInstance)
  #tmpsorted<- sort(tmp, decreasing = FALSE)
  
 # table <- as.data.frame(table(tmpsorted))
  #select <- table(which((as.numeric(table$tmpsorted) <= numP)&(table$Freq >= 2)))
  
  #pAsNeigbour <- which(unique(tmpsorted) <= numP)
  #ov <- length(pAsNeigbour)/a *100
  #return(length(pAsNeigbour))
  return(undTrain)
}
