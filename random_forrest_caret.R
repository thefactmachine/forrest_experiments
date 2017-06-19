rm(list=ls(all=TRUE))


library(RCurl)
library(caret)
library(pROC)
library(randomForest)

x <- RCurl::getURL("https://raw.githubusercontent.com/thefactmachine/random-forest/master/mTitanicAge.csv")
df_titanic <- read.csv(text = x)

# convert to a factor
df_titanic$survived <- factor(df_titanic$survived, levels = c(0,1), labels = c('perished', 'survived'))

set.seed(13)

nrow(df_titanic)

vct_train <- sample(1:nrow(mTitanicAll), 1100)

# set a sort order
ord <- c(2,1,3,4,5,6,7,8,9,10)

df_train <- df_titanic[vct_train, ord]

df_test <- df_titanic[-vct_train, ord]

print(Sys.time())
set.seed(123)

# create a named list -- create a data frame from all combinations of factor variables.
rf.grid <- base::expand.grid(mtry = 1:9)

# set up a k-fold cv  
cv.ctrl <- caret::trainControl(method = "repeatedcv", repeats = 2, summaryFunction = twoClassSummary, classProbs = TRUE)

# method is random forest....this shows alll the different types of models: names(getModelInfo())
rf.tune <- caret::train(survived~ . , data = df_train, method = "rf", metric = "ROC", tuneGrid = rf.grid, trControl = cv.ctrl)

rf.tune
plot(rf.tune)

predClass <- predict(rf.tune, newdata = df_test)
confusionMatrix(data = predClass, df_test$survived)







