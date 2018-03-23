library(caret)
 

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

 
tr <- trainControl(method = "cv", number = 5, returnData = FALSE, trim = TRUE, allowParallel = TRUE)
train(x = train_val2[,-9],y = train_val2[, 9],data=train_val2,method="rf", trControl= tr, proximity = FALSE, norm.votes = TRUE, ntree = 250)

rf.mod<- randomForest(x = train_val2[,-9],y = train_val2[, 9], norm.votes = TRUE, proximity = FALSE, ntree = 500)
 
  