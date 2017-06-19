

# Short for Classification and Regression Training.  Functions for data-splitting
# pre-processing; feature selection; model tuning using re-sampling; variable importance
# http://topepo.github.io/caret/index.html
library(caret)

# webinar for caret:  https://www.youtube.com/watch?v=7Jbb2ItbTC4
# R machine learning toolbox:  https://www.datacamp.com/courses/machine-learning-toolbox



# A collection of artificial and real-world machine learning benchmark problems,
library(mlbench)

# Multivariate regression methods Partial Least Squares Regression (PLSR),
library(pls)

# display and analyse ROC curves
library(pROC)

library(e1071)

# Sonar is from mlbench
data(Sonar)

#caret::createDataPartition. Creates test / training partitons
#Sonar is 208 x 60. The dep variable is Sonar$Class values M = Metal, R = Rock
#returns 157 x 1 matrix. (Rows = .75 x 208 = 157). 

# see also createFolds(y, k = 10, list = TRUE, returnTrain = FALSE)

nrow(Sonar) == 208

set.seed(107)
inTrain <- caret::createDataPartition(y = Sonar$Class, p = .75, list = FALSE)


#Training M = 84, R = 73
training <- Sonar[inTrain,]

#Testing M=27, R = 24
testing <- Sonar[-inTrain,]

#uses pls (partial least squares discriminant analysis)
#scales to mean 0, sd =1
#default is to tune the model over 3 values of each tuning parameter
#resampling uses simply bootstrap by default
#default performance is Kappa statistic; regression = R^2
plsFit <- train(Class ~ .,  data = training, method = "pls", 
                preProc = c("center", "scale"))


#tuneLength is an integer denoting the number of levels for each tuning
# parameter that should be generated. 

#repeatedcv is used tp specify repeated kfold cv. The argument repeats
#controls the number of repetitions. K is controlled by the number argument
#and this defaults to 10.

ctrl <- caret::trainControl(method = "repeatedcv", repeats = 3)


plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, 
                trControl = ctrl, preProc = c("center", "scale"))

#the summaryFunction is used to pass in a function, that takes the observed
#and predicated values and then estimates some measure of performance.
#twoClassSummary will compute measures specific to two-class problems
#such as the area under the ROC curve (the sensitivity and specificity)

#The ROC is based on the predicted class probabilities. These are not computed
#automatically.  Therefore another option is required: classProbs = TRUE

ctrl <- trainControl(method = "repeatedcv", repeats = 3, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)

#since we are using custom performance measures, the criterion that must 
#be optimised should also be specified. The metric = ROC does this.

plsFit <- train(Class ~ .,data = training, method = "pls", tuneLength = 15, 
                trControl = ctrl, metric = "ROC", 
                preProc = c("center", "scale"))

plsFit 

plot(plsFit)

#this is used to predict
plsClasses <- predict(plsFit, newdata = testing)
#produces levels...R, M, M, R, M, R

#we can predict probabilities with the type = prob
plsProbs <- predict(plsFit, newdata = testing, type = "prob")

#we can produce a confusion matrix.....
caret::confusionMatrix(data = plsClasses, testing$Class)

