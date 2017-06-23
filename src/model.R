
# Benchmark: Base model p = 0.50

logLoss(as.integer(train$Made_Donation_in_March_2007) - 1, rep(0.50, dim(train)[1]))
# loss = 0.6931472

# Benchmark 2: p = 0.2636656 (estimated probability of donating)
logLoss(as.integer(train$Made_Donation_in_March_2007) - 1, rep(0.7363344, dim(train)[1]))
# loss = 0.5598477

# Fit logistic model
set.seed(123)
log_model <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = train,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# Fit CART
set.seed(123)
rpart <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = train,
  # tuneLength = 1,
  method = "rpart",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# Save prediction

# write_csv()

# Fit random forest - ranger
set.seed(123)
rf <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = train,
  # tuneLength = 1,
  method = "ranger",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# Fit random forest - randomForest
set.seed(123)
rf <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = train,
  # tuneLength = 1,
  method = "rf",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

