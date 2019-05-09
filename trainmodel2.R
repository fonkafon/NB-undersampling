#train with cross validation
#detach("package:RSNNS", unload=TRUE)
trainmodel <- function(traindata,m){
  flds <- createFolds(traindata$label, k = 10, list = TRUE, returnTrain = FALSE) #10-fold 
  model <- caret::train(label~., traindata, method= m, tuneLength = 5, 
                 trControl = trainControl(
                   method="cv", indexOut=flds, classProbs = TRUE))
  if(m == 'mlp'){detach("package:RSNNS", unload=TRUE)}
  return(model)
}