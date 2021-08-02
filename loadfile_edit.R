loadIndFile <- function(fldfile, setNo){         #folder/file w/o number 
  #for (i in 1:setNo){
    #if(i ==25|i ==32) next
    #if(i==1){dfAll <- data.frame()}
    fname <- paste0(fldfile, setNo,'.csv')
    df <- read.csv(fname,sep=',',header = FALSE)
    cat (fname,'<' ,'Nrow: ', nrow(df), '| Ncol: ',ncol(df),'>\n')
    k <- length(df)
    names(df)[k] <- 'label'
    df[,-k] = scale(as.numeric(unlist(df[,-k])))
  #write.csv(dfAll,file = 'allSets.csv')
  return(df)
}