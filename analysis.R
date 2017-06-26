# hr analysis
  
  #setwd("~/Documents/code/R/hr_analytics") #OSX
  setwd("~/__code/__R/hr_analytics")      #WIN
  
  dt <- read.csv("HR_comma_sep.csv", header = TRUE)
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(GGally)
  
  library(AppliedPredictiveModeling)
  library(caret)

# exploratory data analysis

  str(dt)
  summary(dt)
  
  namez <- colnames(dt)
  dt$left <- as.factor(dt$left)
  
  #for (i in colnames(dt)){
  #    cmd <- paste0('table_',i,'<-data.frame(table(dt$',i,'))')
  #    eval(parse(text = cmd))
  #    cmd1 <- paste0('table_',i,'$perc <- round(table_',i,'$Freq/sum(table_',i,'$Freq)*100,digits = 1)')
  #    eval(parse(text = cmd1))
  #    cmd2 <- paste0('pie(table_',i,'$perc, labels = table_',i,'$Var1,main = "',i,'")')
  #    eval(parse(text = cmd2))
  #}
  
# predictors
  
  predictors <- colnames(dt)
  classif <- c("left")
  predictors <- setdiff(predictors,classif)
  
  dt_predictors <- dt[,predictors]
  dt_class <- dt[,classif]
  
# create dummy variables - dataframe must be numeric  
  
  dt_dum <- dummyVars( ~ ., data = dt_predictors)
  head(predict(dt_dum, newdata = dt_predictors))
  dt_new <- predict(dt_dum, newdata = dt_predictors)
  
# find nearzerovariance descriptor - necessary in order to avoid biased trainings
  
  nzv <- nearZeroVar(dt_new, saveMetrics= TRUE)
  nzv
  nzv <- nearZeroVar(dt_new)
  dt_nzv <- dt_new[, -nzv]
  
# find correlated predictors - reduce reduntant info
  
  correl <- cor(dt_nzv)
  
  high_correl <- findCorrelation(correl, cutoff = .75)
  dt_uncorr <- dt_nzv[,-high_correl]
  
# find linear combinations of predictors 
  
  findLinearCombos(dt_uncorr)
  
# scale values - find training and test
  
  dt1 <- dt_uncorr
  
  set.seed(96)
  inTrain <- sample(seq(along = dt_class), length(dt_class)/2)
  
  training <- dt1[inTrain,]
  test <- dt1[-inTrain,]
  train_thru <- dt_class[inTrain]
  test_thru <- dt_class[-inTrain]
  
  preProcValues <- preProcess(training, method = c("center", "scale"))
  
  trainTransformed <- predict(preProcValues, training)
  testTransformed <- predict(preProcValues, test)
  
  
  
  
  
  #plotSubset <- data.frame(scale(dt_predictors[, c("average_montly_hours", "satisfaction_level")])) 
  #xyplot(average_montly_hours ~ satisfaction_level,
  #       data = plotSubset,
  #       groups = dt_class, 
  #       auto.key = list(columns = 2)) 
  #transformed <- spatialSign(plotSubset)
  #transformed <- as.data.frame(transformed)
  #xyplot(average_montly_hours ~ satisfaction_level, 
  #       data = transformed, 
  #       groups = dt_class, 
  #       auto.key = list(columns = 2)) 