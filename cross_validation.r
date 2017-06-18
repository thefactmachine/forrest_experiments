library(ISLR)
library(boot)
library(dplyr)

plot(mpg ~ horsepower,data = ISLR::Auto)
nrow(ISLR::Auto) == 392
set.seed(123)
vct_random_row <- sample(nrow(ISLR::Auto))

# ============= K fold cross validation =================

# folds <- cut(seq(1,nrow(ISLR::Auto)),breaks=10,labels=FALSE)

#  Schau dir das an  1,1,1,1...2,2,2,....3,3,3,3 ..... 10
vct_folds <- cut(1:nrow(ISLR::Auto), breaks = 10, labels = FALSE)
table(vct_folds)

# Matrixvorbereitung
mat_result <- matrix(rep(NA, 10 * 10), nrow = 10, ncol = 10)
rownames(mat_result) <- paste0("poly_", 1:10)
colnames(mat_result) <- paste0("cv_", 1:10)

for (int_degree in 1:10) {
    for(int_k_fold in 1:10) {
      
      # row numbers for test set
      vct_test <- vct_random_row[vct_folds == int_k_fold]
      
      # test is about 40 rows
      df_test <- ISLR::Auto[vct_test, ]  
      
      # train is about 352 rows
      df_train <- ISLR::Auto[-vct_test,]
      
      # fit model to training set - using n degree polynomial
      model_fit_train <- lm(mpg ~ poly(horsepower, int_degree), data = df_train)
      
      # make predictions on the test set prediction is mpg
      vct_predict <- predict(model_fit_train, df_test)
      
      # calculate mean squared error
      flt_mse_test <- mean((vct_predict - df_test$mpg) ^ 2)
     
       # stick result in the matrix
      mat_result[int_degree, int_k_fold] <- flt_mse_test
    }
}
apply(mat_result, MARGIN = 1, mean)

# ============= Leave one out cross validation =================
vct_random_row
mat_result_loocv <- matrix(rep(NA, 5 * nrow(ISLR::Auto)), nrow = 5, ncol = nrow(ISLR::Auto))
rownames(mat_result_loocv) <- paste0("poly_", 1:5)
colnames(mat_result_loocv) <- paste0("cv_", 1:392)


for (int_degree in 1:5) {
  print(paste("This is the current degree:", int_degree))
  for(int_loo in 1:nrow(ISLR::Auto)) {
    
    # test is 1 row
    df_test <- ISLR::Auto[int_loo, ] 
    
    # train is 392 - 1 ==> 391 rows
    df_train <- ISLR::Auto[-int_loo, ] 
    
    # fit model to training set
    model_fit_train <- lm(mpg ~ poly(horsepower, int_degree), data = df_train)
    
    # make predictions on the test set ==> 1 row
    vct_predict <- predict(model_fit_train, df_test)
    
    # calculate mean squared error
    flt_mse_test <- mean((vct_predict - df_test$mpg) ^ 2)
    
    # stick result in the matrix
    mat_result_loocv[int_degree, int_loo] <- flt_mse_test
    } # int_loo
} # int_degree

vct_actual_result <- apply(mat_result_loocv, MARGIN = 1, mean) %>% round(., 2)
vct_actual_result

# see Hastie page 193
vct_expected_result <- c(24.23, 19.25, 19.33, 19.42, 19.03)
vct_expected_result


all(vct_actual_result == vct_expected_result)









