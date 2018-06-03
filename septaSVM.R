#-- SVM -------------------------------------------------------------------------------------------
# install.packages("caret")
# install.packages("proxy")
# install.packages("pROC")
# install.packages("microbenchmark")
# install.packages("doParallel")
library(tidyverse)
library(skimr)
library(doParallel)
library(caret)
library(proxy)
library(microbenchmark)
library(pROC)

setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-")
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"


#--- Split Train/Test -----------------------------------------------------------------------------
# import saved data
model_data <- readRDS(paste(location,"model_data_dummies.RDS", sep="/"))

# svm needs the class labels to be a labelled factor
model_data$delay <- factor(model_data$delay, labels = c("on.time","late"), levels = c(1, 2))

# Split into training/testing set
set.seed(100)
index <- sample(nrow(model_data), 0.7*nrow(model_data), replace=F)

model_train <- model_data[ index,]
model_test  <- model_data[-index,]

# load training subsets (must run "subsample sets" code first)
test_train_05 <- readRDS(paste(location,"test_train_05.RDS", sep="/"))
test_train_10 <- readRDS(paste(location,"test_train_10.RDS", sep="/"))
test_train_20 <- readRDS(paste(location,"test_train_20.RDS", sep="/"))
test_train_30 <- readRDS(paste(location,"test_train_30.RDS", sep="/"))
test_train_40 <- readRDS(paste(location,"test_train_40.RDS", sep="/"))
test_train_50 <- readRDS(paste(location,"test_train_50.RDS", sep="/"))

data <- list(test_train_05, test_train_10, test_train_20,
             test_train_30, test_train_40, test_train_50)



#-- subsample sets  -------------------------------------------------------------------------------
# initialize
pct <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
samplenames <- c("test_train_05", "test_train_10", "test_train_20",
                 "test_train_30", "test_train_40", "test_train_50")
samplelist <- vector(length=6)
names(samplelist) <- samplenames

for (j in 1:length(pct)) {
  set.seed(0)
  index <- sample(nrow(model_train), pct*nrow(model_train), replace=F)
  test_train <- model_train[index,]
  i = 1

  # loop until we have variation in our data
  # if any column == 0 or any column == 1, then they have no variation and we need to retry
  while ( any(colSums(test_train[,2:75]) == 0) || any(colSums(test_train[,2:75]) == 1) ) {
    set.seed(i)
    index <- sample(nrow(model_train), pct[j]*nrow(model_train), replace=F)
    test_train <- model_train[index,]
    i <- i+1
    print(i)
  } # end while
  assign(paste0("temp",j), test_train)
}


### save training subsets
saveRDS(temp1, paste(location,"test_train_05.RDS", sep="/"))
saveRDS(temp2, paste(location,"test_train_10.RDS", sep="/"))
saveRDS(temp3, paste(location,"test_train_20.RDS", sep="/"))
saveRDS(temp4, paste(location,"test_train_30.RDS", sep="/"))
saveRDS(temp5, paste(location,"test_train_40.RDS", sep="/"))
saveRDS(temp6, paste(location,"test_train_50.RDS", sep="/"))

rm(i, j, index, pct, samplelist, samplenames, test_train, temp1, temp2, temp3, temp4, temp5, temp6)




#-- build & tune model(s) -------------------------------------------------------------------------

### create parallel environment
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
# don't forget to close cl after models finish!

# trainControl controls nuances of train()
tCtrl <- trainControl(method = "cv",
                      number = 5,
                      ## Estimate class probabilities
                      classProbs = TRUE, # must be True for twoClassSummary
                      ## Evaluate performance using the following function
                      summaryFunction = twoClassSummary,
                      ## parallelize!
                      allowParallel = T)


### linear kernel

# use grid search (each row is hyperparam)
linearGrid <- expand.grid(C = c(0, 4^(1:5))) # 'Cost' (0:1024)
nrow(linearGrid)

model_linear <- train(delay ~ .,
                      data = test_train_05,
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
radialGrid <-  expand.grid(sigma = 10 ^(-4:0), #'Sigma' (.0001 : 1)
                           C = c(0, 4^(1:5))) # 'Cost' (0:1024)
nrow(radialGrid)

model_radial <- train(delay ~ .,
                      data = test_train_05,
                      method = "svmRadial",
                      trControl = tCtrl,
                      verbose = FALSE,
                      ## grid search
                      tuneGrid = radialGrid,
                      ## Specify which metric to optimize
                      metric = "ROC")
model_radial
### Notes:
# sigma = .001
# C = 128

# plot grid results
trellis.par.set(caretTheme())
plot(model_radial, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")


### polynomial kernel

# use grid search (each row is hyperparam)
polyGrid <-  expand.grid(degree = seq(2, 3), # 'Polynomial Degree' (^2, ^3)
                         scale = 10 ^((1:4) - 4), # 'Scale' (.001 : 100)
                         C = c(0, 4^(1:5))) # 'Cost' (0:1024)
nrow(polyGrid)

model_poly <- train(delay ~ .,
                    data = test_train_05,
                    method = "svmPoly",
                    trControl = tCtrl,
                    verbose = FALSE,
                    ## grid search
                    tuneGrid = polyGrid,
                    ## Specify which metric to optimize
                    metric = "ROC")
model_poly
### Notes:
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
linearGrid <- expand.grid(C = c(75, 125, 175, 225)) # 'Cost'
nrow(linearGrid)

radialGrid <-  expand.grid(sigma = 0.001, #'Sigma'
                           C = c(75, 125, 175, 225)) # 'Cost'
nrow(radialGrid)


# run hybrid bootstrap/cv
j = 1 # will need this inside the loop
for (i in 1:5) {

  # create mini testing sets with variation
  set.seed(0)
  index <- sample(nrow(model_train), 0.05*nrow(model_train), replace=F)
  test_train <- model_train[index,]


  # if any column == 0 or any column == 1, then they have no variation and we need to retry
  while ( any(colSums(test_train[,2:76]) == 0) || any(colSums(test_train[,2:76]) == 1) ) {
    set.seed(j)
    index <- sample(nrow(model_train), 0.05*nrow(model_train), replace=F)
    test_train <- model_train[index,]
    j <- j+1
    print(j)
  } # end while


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

  # plot grid results
  print(model_linear$finalModel)
  trellis.par.set(caretTheme())
  plot(model_linear, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")


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
  print(model_radial$finalModel)
  trellis.par.set(caretTheme())
  plot(model_radial, metric = "ROC") # "ROC" is parameter (alternative, "Kappa")

} # end for

rm(j, model_linear, index, test_train, model_radial)

### stop parallel environment
stopCluster(cl)


#-- build models with best hyperparam -------------------------------------------------------------

# parallelize
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)


tCtrl <- trainControl(method = "none",
                      classProbs = TRUE,
                      ## parallelize!
                      allowParallel = T)

### Linear ###

# set up structure for runtime analysis
rows <- c(6118, 12236, 24472, 36708, 48944)
times <- vector(length = length(rows))

for (j in 1:length(data)) {
  times[j] <- system.time({
    model_linear <- train(delay ~ .,
                          data = data[[j]],
                          method = "svmLinear",
                          trControl = tCtrl,
                          verbose = FALSE,
                          ## Only a single model can be passed to the function when no resampling is used:
                          tuneGrid = data.frame(C = 4),
                          metric = "ROC")
  })[3] # take elapsed time

  # save
  assign(paste0("model_linear_",j), model_linear)
  saveRDS(get(paste0("model_linear_",j)),
          paste(location, paste0("model_linear_",j,".RDS"), sep="/"))
  cat("Model", j, "\n") # progress update

} # end for

type <- rep("Linear", times=length(rows))
df <- data.frame(rows, times, type)


### Radial ###

# set up structure for runtime analysis
rows <- c(6118, 12236, 24472, 36708, 48944, 61180)
times <- vector(length = length(rows))
#for (j in 1:length(data)) {
  times[j] <- system.time({
    model_radial <- train(delay ~ .,
                          data = data[[j]],
                          method = "svmRadial",
                          trControl = tCtrl,
                          verbose = FALSE,
                          ## Only a single model can be passed to the function when no resampling is used:
                          tuneGrid = data.frame(sigma = .001, C = 75),
                          metric = "ROC")
  })[3] # take elapsed time

  assign(paste0("model_radial_",j), model_radial)
  saveRDS(get(paste0("model_radial_",j)),
          paste(location, paste0("model_radial_",j,".RDS"), sep="/"))
  cat("Model", j, "\n") # progress update

#} # end for

type <- rep("Radial", times=length(times))
df2 <- data.frame(rows, times, type)



#-- plot system times ------------------------------------------------------------------------------
df <- rbind(df, df2) %>%
  group_by(type)

saveRDS(df, paste(location, paste0("runtimes.RDS"), sep="/"))

ggplot(data=df, aes(x=rows, y=times, color=type)) +
  geom_line() +
  geom_point()


### stop parallel environment
stopCluster(cl)



#-- compare model(s) ------------------------------------------------------------------------------

# read in from save
model_linear <- readRDS(paste(location,"model_linear_1.RDS", sep="/"))
model_radial <- readRDS(paste(location,"model_radial_1.RDS", sep="/"))

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

# read in from save
model_linear_05 <- readRDS(paste(location,"model_linear_1.RDS", sep="/"))
model_linear_10 <- readRDS(paste(location,"model_linear_2.RDS", sep="/"))
model_linear_20 <- readRDS(paste(location,"model_linear_3.RDS", sep="/"))
model_linear_30 <- readRDS(paste(location,"model_linear_4.RDS", sep="/"))
model_linear_40 <- readRDS(paste(location,"model_linear_5.RDS", sep="/"))

model_radial_05 <- readRDS(paste(location,"model_radial_1.RDS", sep="/"))
model_radial_10 <- readRDS(paste(location,"model_radial_2.RDS", sep="/"))
model_radial_20 <- readRDS(paste(location,"model_radial_3.RDS", sep="/"))
model_radial_30 <- readRDS(paste(location,"model_radial_4.RDS", sep="/"))
model_radial_40 <- readRDS(paste(location,"model_radial_5.RDS", sep="/"))
model_radial_50 <- readRDS(paste(location,"model_radial_6.RDS", sep="/"))

# check to make sure models read in properly
predict(model_linear_05, newdata = head(model_test), type = "raw")
predict(model_linear_05, newdata = head(model_test), type = "prob")

predict(model_radial_05, newdata = head(model_test), type = "raw")
predict(model_radial_05, newdata = head(model_test), type = "prob")


# Predicting on testing set  (type = c("class","prob"))
pred_linear_05 <- predict(model_linear_05, model_test, type = "raw")
pred_linear_10 <- predict(model_linear_10, model_test, type = "raw")
pred_linear_20 <- predict(model_linear_20, model_test, type = "raw")
pred_linear_30 <- predict(model_linear_30, model_test, type = "raw")
pred_linear_40 <- predict(model_linear_40, model_test, type = "raw")

pred_radial_05 <- predict(model_radial_05, model_test, type = "raw")
pred_radial_10 <- predict(model_radial_10, model_test, type = "raw")
pred_radial_20 <- predict(model_radial_20, model_test, type = "raw")
pred_radial_30 <- predict(model_radial_30, model_test, type = "raw")
pred_radial_40 <- predict(model_radial_40, model_test, type = "raw")
pred_radial_50 <- predict(model_radial_50, model_test, type = "raw")

# Checking classification accuracy
### Linear
# accuracy
mean(pred_linear_05 == model_test$delay)
mean(pred_linear_10 == model_test$delay)
mean(pred_linear_20 == model_test$delay)
mean(pred_linear_30 == model_test$delay)
mean(pred_linear_40 == model_test$delay)

# confusion matrix pcts
prop.table(table(pred_linear_05, model_test$delay))
prop.table(table(pred_linear_10, model_test$delay))
prop.table(table(pred_linear_20, model_test$delay))
prop.table(table(pred_linear_30, model_test$delay))
prop.table(table(pred_linear_40, model_test$delay))

# confusion matrix details
confusionMatrix(pred_linear_05,model_test$delay)
confusionMatrix(pred_linear_10,model_test$delay)
confusionMatrix(pred_linear_20,model_test$delay)
confusionMatrix(pred_linear_30,model_test$delay)
confusionMatrix(pred_linear_40,model_test$delay)


### Radial
# accuracy
mean(pred_radial_05 == model_test$delay)
mean(pred_radial_10 == model_test$delay)
mean(pred_radial_20 == model_test$delay)
mean(pred_radial_30 == model_test$delay)
mean(pred_radial_40 == model_test$delay)
mean(pred_radial_50 == model_test$delay)

# confusion matrix pcts
prop.table(table(pred_radial_05, model_test$delay))
prop.table(table(pred_radial_10, model_test$delay))
prop.table(table(pred_radial_20, model_test$delay))
prop.table(table(pred_radial_30, model_test$delay))
prop.table(table(pred_radial_40, model_test$delay))
prop.table(table(pred_radial_50, model_test$delay))

# confusion matrix details
confusionMatrix(pred_radial_05,model_test$delay)
confusionMatrix(pred_radial_10,model_test$delay)
confusionMatrix(pred_radial_20,model_test$delay)
confusionMatrix(pred_radial_30,model_test$delay)
confusionMatrix(pred_radial_40,model_test$delay)
confusionMatrix(pred_radial_50,model_test$delay)

# optional: get probabilities
pred_linear_05 <- predict(model_linear_05, model_test, type = "prob")
pred_radial_05 <- predict(model_radial_05, model_test, type = "prob")

# ROC curve
roc_radial_05 <- roc(model_test$delay, as.numeric(pred_radial_05), plot=T)
roc_radial_10 <- roc(model_test$delay, as.numeric(pred_radial_10), plot=T)
roc_radial_20 <- roc(model_test$delay, as.numeric(pred_radial_20), plot=T)
roc_radial_30 <- roc(model_test$delay, as.numeric(pred_radial_30), plot=T)
roc_radial_40 <- roc(model_test$delay, as.numeric(pred_radial_40), plot=T)
roc_radial_50 <- roc(model_test$delay, as.numeric(pred_radial_50), plot=T)




# stop parallel environment in case didn't close before
stopCluster(cl)
