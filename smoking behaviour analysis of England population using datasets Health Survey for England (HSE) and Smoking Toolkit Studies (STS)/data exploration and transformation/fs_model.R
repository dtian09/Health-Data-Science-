library(caret)
#feature selection using the AUC of a classifier
#train the model
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
model <- train(df2009_2[, -(ncol(df2009_2)-1)], df2009_2[,ncol(df2009_2)-1], method = "rf", trControl = control)
# estimate variable importance
importance <- varImp(model, useModel = T)
