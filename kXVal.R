kXVal <- function (k=10, actual, err = c("MAD","MSE","RMSE"), FUN, data, ...) {
  # Run k-fold cross-validation on function
  # Args:
    # k - number cross-validation partitions
    # actual - vector of actual classifications for entire dataset
    # err - choose MAD, MSE, RMSE
    # FUN - function name
    # data - dataset (with independent / Y variable included)
    # ... - other arguments to be passed to FUN

  ### prepare subsequent function call
  fnCall <- match.call(expand=TRUE)
    # passes all parameters to subsequent function.
  # Next function doesn't need k, actual, FUN, so set to NULL
  fnCall$k <- NULL
  fnCall$actual <- NULL
  fnCall$err <- NULL
  fnCall[[1]] <- fnCall$FUN # we do need the subsequent function name though, so preserve
  fnCall$FUN <- NULL

  err <- match.arg(err)

  ### load error functions
  source("MAD.r")
  source("MSE.r")
  source("RMSE.r")

  ### run k-fold cross-validation
  if (k < 2) {
    stop('k < 2; set k >= 2')
  } else {
    # initialize storage for errors
    error <- rep(0,k)
    # error <- vector("list",k)

    # assign each row to one of k groups
    assignment <- ceiling( runif(nrow(data), min=0, max=k) )

    for (i in 1:k) {
      actl <- actual[assignment==i]
      test <- data[assignment == i,]
      train <- data[assignment != i,] # training data is everything aside from i_th group

      model <- eval(fnCall, parent.frame())
      preds <- predict(model, newdata=test)

      # determine error function to use
      if (err == "MSE") {
        error[i] <- MSE(actual=actl, prediction=preds)
      } else if (err == "RMSE") {
        error[i] <- RMSE(actual=actl, prediction=preds)
      } else { # use MAD as default
        error[i] <- MAD(actual=actl, prediction=preds)
      }

    } # end for
  } # end else

  # return the average error across k folds
  return( mean(error) )
}
