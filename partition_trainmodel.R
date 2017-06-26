  # DATA PARTITIONING and MODEL TRAINING

  data_preprocessed <- dt_uncorr

  trainIndex <- createDataPartition(dt_class, p = .75, list = FALSE, times = 1)
  
  training <- as.data.frame(data_preprocessed[trainIndex,])
  test <- as.data.frame(data_preprocessed[-trainIndex,])
  
  train_class <- dt_class[trainIndex]
  test_class <- dt_class[-trainIndex]
  
  training$class <- train_class
  test$class <- test_class
  
  down_train <- downSample(x = training[, -ncol(training)],
                           y = training$class )
  
  # parameter tuning - set cross validation method
  
  fitControl <- trainControl(method = "repeatedcv", repeats = 5,
                             classProbs = TRUE,
                             summaryFunction = twoClassSummary)
  
  # select and train model
  
  set.seed(851)
  
  modelfit4 <- train(Class ~ ., 
                     data = down_train, 
                     method = "xgbTree", 
                     trControl = fitControl, 
                     verbose = TRUE,
                     preProcess = "pca",
                     metric = "Accuracy")
  
  