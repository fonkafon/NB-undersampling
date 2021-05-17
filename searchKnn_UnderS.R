#search knn #caret
searchKnn <- function(dfT){
  for (i in 1: nrow(dfT)){
    set.seed(seed)
    k<-(sqrt(imb))+sqrt(nrow(train))
    kNN <- knn3Train(as.matrix(dfT[-i,-ncol(dfT)]), as.matrix(dfT[i,-ncol(dfT)]), 
                     cl = as.matrix(dfT[-i,]$label), k = k)
    #kNN <- knn3Train(df[-i,-ncol(df)], df[i,-ncol(df)], cl = df[-i,]$label, k = k)
    prob <- attr(kNN,'prob')
    ifelse(i==1,allprob <- prob, allprob <- rbind(allprob,prob))
  }
  dfT$probN <- allprob[,1]
  dfT$probP <- allprob[,2]
  tmpN <- dfT[(dfT$probN==1)&(dfT$label=='negative'),] 
  tmpP <- dfT[dfT$label=='positive',]
  dfT <- rbind(tmpP,tmpN)
  
  dfT$probP <- NULL
  dfT$probN <- NULL
  return(dfT)
}
