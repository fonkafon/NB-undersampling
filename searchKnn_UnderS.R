#search knn #caret
searchKnn <- function(dfT,knn){
  for (i in 1: nrow(dfT)){
    set.seed(seed)
    kNN <- knn3Train(as.matrix(dfT[-i,-ncol(dfT)]), as.matrix(dfT[i,-ncol(dfT)]), 
                     cl = as.matrix(dfT[-i,]$label), k = knn)
    #kNN <- knn3Train(df[-i,-ncol(df)], df[i,-ncol(df)], cl = df[-i,]$label, k = k)
    prob <- attr(kNN,'prob')
    ifelse(i==1,allprob <- prob, allprob <- rbind(allprob,prob))
  }
  dfT$probN <- allprob[,1]
  dfT$probP <- allprob[,2]
  return(dfT)
}
