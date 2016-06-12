train_lm <- function(DF, error_est = "none", model = "rpart", 
                     y_ind, x_ind, par = FALSE, form.f = " "){
  library(caret)
  set.seed(3456)
  
  ##########################
  #   Parallel processing
  if(par){
  
    require(foreach); require(doParallel)
    cl <- makeCluster(3) #use 3 cores
    registerDoParallel(cl)
  }
  
  ################ 
  #  remove NA
  DF <- na.omit(DF)
  
  ################
  #  split data to x and y
  names(DF)[y_ind] <- "outcome"
  y_data <- DF[, y_ind]
  x_data <- DF[, x_ind]
  
  ################
  #  Create dummmy variables
  # before splitting the data.
  # Otherwise train function will create dummies on the fly. But, in this case, training set will not
  # have the same number of levels for each factor. This will create different number of predictros.
  # The same is true for cross-validation. 
  
  if(form.f != " "){
    dv <- dummyVars( form.f , data = x_data)
  }else{
    dv <- dummyVars( ~ . , data = x_data)  
  }
  
  x_data <- data.frame(predict(dv, x_data))
  
  ### some correction -  color is already TRUE or FALSE. We don't need it dublicated  
  x_data <- x_data %>%
    select(- contains("FALSE"))

  ############### 
  #  Split data to train and test set
  trainIndex <- createDataPartition(y_data$outcome, p = .8, list = F)
  x_data_train <- x_data[ trainIndex,]
  x_data_test  <- x_data[-trainIndex,]
  
  y_data_train <- y_data[ trainIndex,]
  y_data_test <- y_data[ -trainIndex,]
  
  ############### 
  #  Center and scale or range (0 to 1)
  # dummyVars comand created dummies as num. 
  # If we want to scale data, we either should recode them as factors 
  # or use range comand instead (will map data to interval (0, 1))
  preProcValues <- preProcess(x_data_train, method = c("range"))
  
  x_data_test <- predict(preProcValues, x_data_test)
  x_data_train <- predict(preProcValues, x_data_train)
  
  ###############
  #  How large is the df now
  cat("New DF dimensions \n", dim(x_data_train), "\n")
  cat("n/p ratio is \n", dim(x_data_train)[1]/dim(x_data_train)[2], "\n")
  
  ###############
  #  Combine back to one df 
  df_train <- data.frame(x_data_train, y_data_train)
  df_test <- data.frame(x_data_test, y_data_test)
  
  ###############
  #  Set up training
  fitControl <- trainControl(method = error_est,
                             number = 10,
  #                           classProbs = TRUE, # TRUE for ROC
  #                           summaryFunction = twoClassSummary, ## twoClassSummary this for ROC metric
                             verbose = TRUE)

  ###############
  #  Train the model
  lmFit <- train(outcome ~ ., data = df_train,
                 method = model,
  #               metric= "ROC", # "ROC" for AUC
  #               preProc = c("scale"),
                 trControl = fitControl)
  if(par){
    stopCluster(cl)
  }

  return(list(model = lmFit, train = df_train, test = df_test))
}