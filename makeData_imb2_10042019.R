#generate random uniformly distributed data
makeData <- function(ovPercent,imb, lastI){
  set.seed(seed)
  x<-runif(6000,1,1000*sqrt(imb))
  y<-runif(6000,0,2000*sqrt(imb))
  #x<-runif(6000,1,500*sqrt(imb))
  #y<-runif(6000,0,100*sqrt(imb))
  # x<-runif(6000,1,100*sqrt(imb))
  # y<-runif(6000,0,200*sqrt(imb))
  dfN <- data.frame(x,y)
  dfN$label <- as.factor('negative')
  #plot(dfN[,1:2])
  
  ov <- ovPercent/2
  set.seed(1)
   x<-runif(6000/imb,1000*sqrt(imb)+1-20*ov,(1000*sqrt(imb)+1000)-20*ov)
   y<-runif(6000/imb,0,2000)
  #x<-runif(6000/imb,50*sqrt(imb)+1-ov,(50*sqrt(imb)+50)-ov)
  #y<-runif(6000/imb,0,100)
   #x<-runif(6000/imb,100*sqrt(imb)+1-2*ov,(100*sqrt(imb)+100)-2*ov)
   #y<-runif(6000/imb,0,200)
  dfP <- data.frame(x,y)
  dfP$label <- as.factor('positive')
  #plot(dfP[,1:2])
  df <- rbind(dfP,dfN)     
  
  #if(i == 1){
  #  filenum <- j
  #  fName <- paste0('synData', filenum, '.pdf')
  #  pdf(file = fName)
  #  }
  p<-plot(df[,1:2], col = df$label, main = paste0('overlap = ',ovPercent, '%, imb = ',imb,'%' ))
  #if(i == lastI) {dev.off()}
  return(df)
}
