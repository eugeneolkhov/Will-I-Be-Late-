#-- SVM -------------------------------------------------------------------------------------------

# install.packages("e1071")
# install.packages("kernlab")
# install.packages("pROC")
# install.packages("microbenchmark")
# install.packages("doParallel")
library(tidyverse)
library(skimr)
library(doParallel)
library(caret)
#library(e1071)
#library(kernlab) # ksvm
library(pROC)


location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"


#--- Split Train/Test -----------------------------------------------------------------------------
# import saved data
model_data <- readRDS(paste(location,"model_data.rds", sep="/"))

# svm needs the class labels to be a labelled factor
model_data$delay <- factor(model_data$delay, labels = c("on.time","late"), levels = c(1, 2))

# Split into training/testing set
set.seed(100)
# index <- createDataPartition(model_data$delay, p = .7, list = FALSE, times = 1)
index <- sample(nrow(model_data), 0.7*nrow(model_data), replace=F)

model_train <- model_data[ index,]
model_test  <- model_data[-index,]

#-- subset data -----------------------------------------------------------------------------------

# create mini set for fast testing and hyperparameter search
set.seed(12322)
pct <- .02 # can incrementally increase this
a <- sample(nrow(model_train), pct * nrow(model_train), replace = FALSE)
b <- sample(nrow(model_test), pct * nrow(model_test), replace = FALSE)

test_train <- model_train[a,]
test_test <- model_test[b,]

rm(a,b)

summary(test_train)
summary(test_test)

#-- for time estimates ----------------------------------------------------------------------------

# in general, SVMs take a while... use subset of data to train on
# how long?
#require(microbenchmark)
#times <- microbenchmark(svm(delay ~ ., data = test_train, scale=F, kernel="radial"),
#                        svm(delay ~ ., data = test_train, scale=F, kernel="linear"),
#                        svm(delay ~ ., data = test_train, scale=F, kernel="sigmoid"),
#                        times=5)


#-- build & tune model(s) -------------------------------------------------------------------------

### create parallel environment

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
  # don't forget to close cl after models finish!

# trainControl controls nuances of train()
tCtrl <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 3, # for method = "repeatedcv"
                      ## Estimate class probabilities
                      classProbs = TRUE, # must be True for twoClassSummary
                      ## Evaluate performance using the following function
                      summaryFunction = twoClassSummary,
                      ## parallelize!
                      allowParallel = T)


### linear kernel

# use grid search (each row is hyperparam)
linearGrid <- expand.grid(C = c(0, 2^(0:10))) # 'Cost' (0:1024)
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
# seed = 9876
# C = 128

# plot grid results
trellis.par.set(caretTheme())
plot(model_linear, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")



### radial kernel

# use grid search (each row is hyperparam)
radialGrid <-  expand.grid(sigma = 10 ^(-5:0), #'Sigma' (.00001 : 1)
                           C = c(0, 2^(0:10))) # 'Cost' (0:1024)
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
### Notes:
# (seed 9876)
# sigma = .001
# C = 128

# plot grid results
trellis.par.set(caretTheme())
plot(model_radial, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")


### polynomial kernel

# use grid search (each row is hyperparam)
polyGrid <-  expand.grid(degree = seq(1, 3), # 'Polynomial Degree' (^1, ^2, ^3)
                         scale = 10 ^((1:6) - 4), # 'Scale' (.001 : 100)
                         C = c(0, 2^(0:10))) # 'Cost' (0:1024)
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
### Notes:
# (seed 9876)
# degree = 1
# scale = 0.1
# C = 4

# plot grid results
trellis.par.set(caretTheme())
plot(model_poly, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")


# stop parallel environment
stopCluster(cl)


### compare model(s)

resamps <- resamples(list(linear = model_linear,
                          radial = model_radial,
                          poly = model_poly))
resamps
summary(resamps)

# plot
trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")


difValues <- diff(resamps)
difValues
summary(difValues)

# ROC
#        linear radial     poly
# linear        -0.0022528 -0.0017025
# radial 1                  0.0005503
# poly   1      1
#
# Sens
#        linear radial    poly
# linear        -0.002344 -0.007793
# radial 1                -0.005449
# poly   1      1
#
# Spec
#        linear radial    poly
# linear         0.001258 -0.001048
# radial 1                -0.002306
# poly   1      1

# plot differences
trellis.par.set(caretTheme())
bwplot(difValues, layout = c(3, 1))

#-- build & tune model(s) take 2 ------------------------------------------------------------------
# given that poly isn't much better than radial and takes _forever_,
# we should just compare linear and radial and try to narrow the scope of the hyperparameters


### parallelize
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)


### hybrid bootstrap sample / cv approach

# trainControl controls nuances of train()
tCtrl <- trainControl(method = "cv",
                      number = 5,
                      ## Estimate class probabilities
                      classProbs = TRUE, # must be True for twoClassSummary
                      ## Evaluate performance using the following function
                      summaryFunction = twoClassSummary,
                      ## parallelize!
                      allowParallel = T)

# save grids to search
linearGrid <- expand.grid(C = c(75, 100, 125, 150, 175, 200)) # 'Cost'
nrow(linearGrid)

radialGrid <-  expand.grid(sigma = c(.0015, 10^(-5:-1)), #'Sigma'
                          C = c(75, 100, 125, 150, 175, 200)) # 'Cost'
nrow(radialGrid)

# create empty list to save to
linearList <- list()
# radialList <- list()

# run hybrid bootstrap/cv
for (i in 1:5) {
  pct <- .1 # can incrementally increase this
  a <- sample(nrow(model_train), pct * nrow(model_train), replace = FALSE)
  b <- sample(nrow(model_test), pct * nrow(model_test), replace = FALSE)

  test_train <- model_train[a,]
  test_test <- model_test[b,]

  rm(a,b)

  ### linear kernel
  model_linear <- train(delay ~ .,
                        data = test_train,
                        method = "svmLinear",
                        trControl = tCtrl,
                        verbose = FALSE,
                        ## grid search
                        tuneGrid = linearGrid,
                        ## Specify which metric to optimize
                        metric = "ROC")

  ### radial kernel
  model_radial <- train(delay ~ .,
                        data = test_train,
                        method = "svmRadial",
                        trControl = tCtrl,
                        verbose = FALSE,
                        ## grid search
                        tuneGrid = radialGrid,
                        ## Specify which metric to optimize
                        metric = "ROC")



  # plot grid results
  print(model_linear$finalModel)
  trellis.par.set(caretTheme())
  plot(model_linear, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")

  print(model_radial$finalModel)
  trellis.par.set(caretTheme())
  plot(model_radial, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")

} # end for

### stop parallel environment
stopCluster(cl)


#-- build models with best hyperparam -------------------------------------------------------------

### parallelize
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)


tCtrl <- trainControl(method = "none",
                      classProbs = TRUE,
                      ## parallelize!
                      allowParallel = T)


linear_model <- train(delay ~ .,
                   data = model_train,
                   method = "svmLinear",
                   trControl = tCtrl,
                   verbose = FALSE,
                   ## Only a single model can be passed to the function when no resampling is used:
                   tuneGrid = data.frame(C = 128),
                   metric = "ROC")
linear_model

radial_model <- train(delay ~ .,
                   data = model_train,
                   method = "svmRadial",
                   trControl = tCtrl,
                   verbose = FALSE,
                   ## Only a single model can be passed to the function when no resampling is used:
                   tuneGrid = data.frame(sigma = .01, C = 128),
                   metric = "ROC")
radial_model


# stop parallel environment
stopCluster(cl)


### compare model(s) 2

resamps <- resamples(list(linear = model_linear,
                          radial = model_radial))
resamps
summary(resamps)

# plot
trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")


difValues <- diff(resamps)
difValues
summary(difValues)

# ROC
#        linear radial     poly
# linear        -0.0022528 -0.0017025
# radial 1                  0.0005503
# poly   1      1
#
# Sens
#        linear radial    poly
# linear        -0.002344 -0.007793
# radial 1                -0.005449
# poly   1      1
#
# Spec
#        linear radial    poly
# linear         0.001258 -0.001048
# radial 1                -0.002306
# poly   1      1

# plot differences
trellis.par.set(caretTheme())
bwplot(difValues, layout = c(3, 1))


#-- predict using models --------------------------------------------------------------------------

predict(svm_model, newdata = head(model_test), type = "raw")
predict(svm_model, newdata = head(model_test), type = "class")
predict(svm_model, newdata = head(model_test), type = "prob")











# Predicting on train set  (type = c("class","prob"))
pred_linear <- predict(model_linear, test_train, type = "raw")
pred_radial <- predict(model_radial, test_train, type = "class")

# Checking classification accuracy
prop.table(table(pred_train, train$delay))


# Predicting on Validation set
pred_test <- predict(model_linear, test_test, type = "prob")

# Checking classification accuracy
mean(pred_test == test$delay)
prop.table(table(pred_test,test$delay))


# stop parallel environment
stopCluster(cl)
