library(ISLR)
library(boot)
library(dplyr)

require(randomForest)
require(MASS)
set.seed(101)
# boston is 506 * 14
dim(Boston)

df_boston <- Boston

# produces a sample of 300 values that take the range 1:506
vct_train <- sample(1:nrow(Boston), 300)
?Boston

# fit a random forrest and see how it goes...
rf_boston <- randomForest(medv~.,data = df_boston, subset = vct_train)
rf_boston

# The MSR and % variance explained are based on OOB  or 
# _out-of-bag_ estimates, a very clever device in random forests to get honest error estimates. 
# The model reports that `mtry=4`, which is the number of variables randomly chosen at each split. 
# Since $p=13$ here, we could try all 13 possible values of `mtry`. We will do so, record the results, and make a plot.

# create a vector of 13 zeros
vct_out_of_bag_error <- double(13)

# create a vector of 
vct_test_error <- double(13)


# data set is 506 observations
# training set is 300 observations
# test set is 206 observations

for(mtry in 1:13) {
  
  # Fit a Random Forrest model
  random_forrest_fit <- randomForest(medv~.,data = df_boston, subset = train, mtry = mtry, ntree = 400)
  
  # Calculate out of bag error on training 
  vct_out_of_bag_error[mtry] <- random_forrest_fit$mse[400]
  
  # Calculate a vector of predictions on test 
  vct_predictions <- predict(random_forrest_fit, df_boston[-train,])
  
  # Calculate MSE for predictions on test data
  vct_test_error[mtry] <- with(df_boston[-train,], mean((medv - vct_predictions)^2))
  
  # cat does not put carriage returns after
  cat(mtry," ")

  }


# plot columns of matrices
matplot(1:mtry, # X axis
        cbind(vct_test_error, vct_out_of_bag_error), 
        pch = 19,
        col=c("red","blue"),
        type = "b",
        ylab = "Mean Squared Error")


legend("topright",legend=c("Test erro","Out of bag err"),pch=19,col=c("red","blue"))

# Not too difficult! Although the test-error curve drops below the OOB curve, 
# these are estimates based on data, and so have their own standard errors 
# (which are typically quite large). Notice that the points at the end with `mtry=13` correspond to bagging.

# error estimates are very correlated and the data sets were not that large. 

# default mtry for classification is sqrt(Number of predictors); regression it is (p / 3)



