#-- SVM -------------------------------------------------------------------------------------------

#install.packages("e1071")
#install.packages("kernlab")
#install.packages("pROC")
#install.packages("microbenchmark")
#install.packages("doParallel")
library(tidyverse)
library(skimr)
library(openxlsx)
library(lubridate)
library(doParallel)
library(caret)
library(e1071)
#library(kernlab) # ksvm
library(pROC)


#-- subset data -----------------------------------------------------------------------------------

# create mini set for fast testing and hyperparameter search
set.seed(9876)
pct <- .02 # can incrementally increase this
a <- sample(nrow(model_train), pct * nrow(model_train), replace = FALSE)
b <- sample(nrow(model_test), pct * nrow(model_test), replace = FALSE)

test_train <- model_train[a,]
test_test <- model_test[b,]

rm(index, a,b)

summary(test_train)
summary(test_test)

#-- for time estimates ----------------------------------------------------------------------------

# in general, SVMs take a while... use subset of data to train on
# how long?
require(microbenchmark)
times <- microbenchmark(svm(delay ~ ., data = test_train, scale=F, kernel="radial"),
                        svm(delay ~ ., data = test_train, scale=F, kernel="linear"),
                        svm(delay ~ ., data = test_train, scale=F, kernel="sigmoid"),
                        times=5)


#-- create parallel environment -------------------------------------------------------------------

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

  # machine learning code goes in here

#-- build & tune model(s) -------------------------------------------------------------------------

# trainControl controls nuances of train()
tCtrl <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 10, # for method = "repeatedcv"
                      ## Estimate class probabilities
                      classProbs = TRUE, # must be True for twoClassSummary
                      ## Evaluate performance using the following function
                      summaryFunction = twoClassSummary,
                      ## parallelize!
                      allowParallel = T)


### linear kernel

# use grid search (each row is hyperparam)
linearGrid <- expand.grid(C = 2^((1:8) - 4)) # 'Cost' (.125 : 16)
nrow(linearGrid)

model_linear <- train(delay ~ .,
                      data = test_train,
                      method = "svmLinear",
                      trControl = tCtrl,
                      verbose = FALSE,
                      ## grid search
                      tuneGrid = linearGrid,
                      ## Specify which metric to optimize
                      metric = "ROC")
model_linear
### Notes:
# C = 4

# plot grid results
trellis.par.set(caretTheme())
plot(model_linear, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")



### radial kernel

# use grid search (each row is hyperparam)
radialGrid <-  expand.grid(sigma = 10 ^((1:6) - 4), #'Sigma' (.001 : 100)
                           C = 2^((1:8) - 4)) # 'Cost' (.125 : 16)
nrow(radialGrid)

model_radial <- train(delay ~ .,
                      data = test_train,
                      method = "svmRadial",
                      trControl = tCtrl,
                      verbose = FALSE,
                      ## grid search
                      tuneGrid = radialGrid,
                      ## Specify which metric to optimize
                      metric = "ROC")
model_radial


# plot grid results
trellis.par.set(caretTheme())
plot(model_radial, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")


### polynomial kernel

# use grid search (each row is hyperparam)
polyGrid <-  expand.grid(degree = seq(1, 3), # 'Polynomial Degree' (^1, ^2, ^3)
                         scale = 10 ^((1:6) - 4), # 'Scale' (.001 : 100)
                         C = 2^((1:8) - 4)) #'Cost' (.125 : 16)
nrow(polyGrid)

model_poly <- train(delay ~ .,
                      data = test_train,
                      method = "svmPoly",
                      trControl = tCtrl,
                      verbose = FALSE,
                      ## grid search
                      tuneGrid = polyGrid,
                      ## Specify which metric to optimize
                      metric = "ROC")
model_poly


# plot grid results
trellis.par.set(caretTheme())
plot(model_poly, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")



#-- compare model(s) ------------------------------------------------------------------------------

resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps
summary(resamps)

# plot
trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")


difValues <- diff(resamps)
difValues
summary(difValues)

# plot differences
trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))










# Predicting on train set  (type = c("class","prob"))
pred_train <- predict(model, train, type = "class")

# Checking classification accuracy
prop.table(table(pred_train, train$delay))


# Predicting on Validation set
pred_test <- predict(model, test, type = "class")

# Checking classification accuracy
mean(pred_test == test$delay)
prop.table(table(pred_test,test$delay))


#-- stop parallel environment ---------------------------------------------------------------------
stopCluster(cl)
