#search knn 
library(FNN)
searchKnn <- function(df,k){
  #k <- 5
  k<-(sqrt(imb))+sqrt(nrow(df))
  #k<- sqrt(nrow(df))
  #sort positive and negative instances
  dfP <- df[df$label=='positive',]
  dfN <- df[df$label=='negative',]
  df <- rbind(dfP, dfN)
  numP <- nrow(dfP) #last row of the negative class
  
  set.seed(seed)
  nn <- get.knn(df[,-ncol(df)],k)   
  nnIndex <- as.data.frame(nn$nn.index)
  freqT <- as.data.frame(cbind((numP+1):nrow(df),0)) #neg only
  
  for (n in 1: numP){ #check NNs of positive
    for (m in 1:dim(nnIndex)[2]){ #dim(nnIndex)[2]==k 
      if(nnIndex[n,m] > numP){#look for neg nn only
        #freqT[nnIndex[n,m],2] <-  freqT[nnIndex[n,m],2] + 1
        freqT[which(freqT[,1]==nnIndex[n,m]),2] <-  freqT[which(freqT[,1]==nnIndex[n,m]),2] + 1
        #ifelse(exists('trainN'), trainN <- rbind(trainN, df[n,]),
        #       trainN <- df[n,])  
      } 
    }
  }
  elimIndex <- freqT[which(freqT[,2]>1),1] 
  elimNeg<- df[elimIndex,]  #negative to be removed in the first search
  if(dim(elimNeg)[1]>0){
    #2nd round search
    set.seed(seed)
    #k2 <- sqrt(nrow(elimNeg))
    #nn2 <- get.knn(df[,-ncol(df)],k2) 
    nnIndex2 <- as.data.frame(nn$nn.index)
    freqT2 <- as.data.frame(cbind((numP+1):nrow(df),0))
    elimIndex <- as.data.frame(elimIndex)
    
    for (n in 1: nrow(elimNeg)){ #check NNs of positive
      nn <- elimIndex[n,] 
      for (m in 1:dim(nnIndex2)[2]){
        if(nnIndex2[nn,m] > numP){#look for neg nn only
          #freqT[nnIndex[n,m],2] <-  freqT[nnIndex[n,m],2] + 1
          freqT2[which(freqT[,1]==nnIndex2[nn,m]),2] <-  freqT2[which(freqT[,1]==nnIndex2[nn,m]),2] + 1
          #ifelse(exists('trainN2'), trainN2 <- rbind(trainN2, df[nn,]),
          #       trainN2 <- df[nn,])  
        } 
      }
    }
   trainNIndex <- freqT[which((freqT[,2]<2)&(freqT2[,2]<2)),1]  
  } else {trainNIndex <- freqT[which(freqT[,2]<2),1]}
  trainN<- df[trainNIndex,]
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
