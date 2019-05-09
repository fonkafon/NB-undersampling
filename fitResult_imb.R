#fit RF vs %OV
fitResult <- function(train,test){
  set.seed(seed)
  result <- fitTest(train, test)
  #result <- rbind(overlap = ovPercent, result)
  return(result)
}
