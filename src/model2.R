
# loss function
logLoss2 <- function (data, lev = NULL, model = NULL) 
{
  probs <- pmax(pmin(as.numeric(data$Donated), 1 - 1e-15), 1e-15)
  actual <- (as.numeric(data$obs) - 1)
  out <- c(mean(actual * log(probs) + (1 - actual) * log(1 - probs))) * -1
  names(out) <- c("LogLoss2")
  out
}

### Split training data ----------------------------------------------------------------------------------
set.seed(123)
trainIndex <- sample(nrow(train), round(0.70 * nrow(train)))
trainSet <- train[trainIndex, ]
testSet <- train[-trainIndex, ]


#### Build models ----------------------------------------------------------------------------------------

### Benchmarks -------------------------------------------------------------------------------------------

# Benchmark: Base model p = 0.50

logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, rep(0.50, dim(testSet)[1]))
# loss = 0.6931472

# Benchmark: p = 0.2506887 (estimated probability of donating)
trainSet %>% select(Made_Donation_in_March_2007) %>% table %>% prop.table()
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, rep(0.7493113, dim(testSet)[1]))
# loss = 0.5499742

### Logistic Model

# Fit logistic model
set.seed(123)
log_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "logLoss",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

log_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "LogLoss2",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)

# logistic model: prediction

log_pred <- predict(log_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, log_pred)

log_mod2 <- train(
  Made_Donation_in_March_2007 ~ sqrt(Number_of_Donations) * Months_since_Last_Donation * Months_since_First_Donation + 
    I(Number_of_Donations > 1) + I(Months_since_Last_Donation > 4) + I(Number_of_Donations > 3) + log(Months_since_First_Donation),
  data = trainSet,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "LogLoss2",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)

log_mod2 <- train(
  Made_Donation_in_March_2007 ~ sqrt(Number_of_Donations) * Months_since_Last_Donation * Months_since_First_Donation + 
    I(Number_of_Donations > 1) + I(Months_since_Last_Donation > 4),
  data = trainSet,
  # tuneLength = 1,
  method = "glm",
  family = "binomial",
  metric = "LogLoss2",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)
# logistic model: prediction

log_pred2 <- predict(log_mod2, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, log_pred2)
# loss = 0.4655694


# Output prediction

submissionFormat$`Made Donation in March 2007` <- predict(log_mod2, test, type = "prob")[1] %>% unlist
log_output <- submissionFormat
write_csv(log_output, "predictions/log_pred5.csv")

### Classification and Regression Trees ----------------------------------------------------------------

# Fit CART
set.seed(123)
rpart_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "rpart",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

rpart_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "rpart",
  family = "binomial",
  metric = "LogLoss2",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)

# CART model: prediction

rpart_pred <- predict(rpart_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, rpart_pred)
# loss = 0.5569772

# Output prediction

submissionFormat$`Made Donation in March 2007` <- predict(rpart_mod, test, type = "prob")[2] %>% unlist
rpart_output <- submissionFormat
write_csv(rpart_output, "predictions/rpart_pred.csv")

### Random Forest -------------------------------------------------------------------------------------

# Fit random forest - ranger
set.seed(123)
ranger_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "ranger",
  metric = "LogLoss2",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)

# ranger model: prediction

ranger_pred <- predict(ranger_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, ranger_pred)
#ranger_pred loss = 0.5343874

# Output prediction

submissionFormat$`Made Donation in March 2007` <- predict(ranger_mod, test, type = "prob")[2] %>% unlist
ranger_output <- submissionFormat
write_csv(ranger_output, "predictions/ranger_pred.csv")


# Fit random forest - randomForest
set.seed(123)
rf_mod <- train(
  Made_Donation_in_March_2007 ~ . -id + sqrt(Number_of_Donations) * Months_since_Last_Donation * Months_since_First_Donation + 
    I(Number_of_Donations > 1) + I(Months_since_Last_Donation > 4) + 
    I((Months_since_First_Donation - Months_since_Last_Donation) / Number_of_Donations) + 
    I(Months_since_Last_Donation / Months_since_First_Donation),
  data = trainSet,
  # tuneLength = 1,
  method = "rf",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# rf model: prediction

rf_pred <- predict(rf_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, rf_pred)
# loss = 0.5343874

# Output prediction

submissionFormat$`Made Donation in March 2007` <- predict(rf_mod, test, type = "prob")[2] %>% unlist
rf_output <- submissionFormat
write_csv(rf_output, "predictions/rf_pred.csv")

# Fit conditional inference tree
set.seed(123)
ctree_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "ctree",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# ctree model: prediction

ctree_pred <- predict(ctree_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, ctree_pred)
# loss = 0.5236968

# Fit C5.0

# Fit conditional inference tree
set.seed(123)
c50_mod <- train(
  Made_Donation_in_March_2007 ~ . -id,
  data = trainSet,
  # tuneLength = 1,
  method = "C5.0",
  metric = "logLoss",
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = mnLogLoss)
)

# ctree model: prediction

c50_pred <- predict(c50_mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, c50_pred)
# loss = 0.5223294

# Fit average neural net model

mod <- train(
  Made_Donation_in_March_2007 ~ . -id + sqrt(Number_of_Donations) * Months_since_Last_Donation * Months_since_First_Donation + 
    I(Number_of_Donations > 1) + I(Months_since_Last_Donation > 4) + 
    I((Months_since_First_Donation - Months_since_Last_Donation) / Number_of_Donations) + 
    I(Months_since_Last_Donation / Months_since_First_Donation),
  data = trainSet,
  # tuneLength = 1,
  method = "avNNet",
  metric = "LogLoss2",
  maximize = FALSE,
  trControl = trainControl(method = "cv", 
                           number = 5, 
                           verboseIter = TRUE,
                           classProbs = TRUE,
                           summaryFunction = logLoss2)
)
pred <- predict(mod, testSet, type = "prob")[2]
logLoss(as.integer(testSet$Made_Donation_in_March_2007) - 1, pred)
