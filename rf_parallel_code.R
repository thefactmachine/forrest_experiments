
library("parallel")
library("foreach")
library("doParallel")
n <- 12 #number of cores to use
cl <- makeCluster(n)
registerDoParallel(cl,cores = n)
#use foreach around the loop you wish to parallelise, you have to pass in the packages to be used and specify how to combine the results (rbind in this case though I suppose you might use cbind)
#the %dopar% switch goes before the opening brace of the parallelised loop.
data <-  foreach( I  = 1 : length(predictors),
                  .packages = c("randomForest","ROCR"),
                  .combine=rbind) %dopar% {
                    psub = predictors[-i]
                    RFs <- randomForest(GRTGT ~ .,
                                        data=trainset[,c(psub, tgtng)],
                                        ntree=coppice, 
                                        sampsize= c(samplesize,samplesize),
                                        importance=T,
                                        na.action=na.omit,
                                        replace=T)
                    NGs_predict <- predict(RFs, testset[, c(psub, tgtng)], type="prob")[,2]
                    NGs_pred <- prediction(NGs_predict,testset[, c(psub, tgtng)]$GRTGT)
                    auc <- performance(NGs_pred, "auc")
                    auc <- as.numeric(auc@y.values)
                    #results combined together, this result will be bound as a row with the other row results by rbind as specified above  
                    out <- cbind(auc ,0.774 - auc ,predictors[i])
                  }
)

#Don’t forget to release the cores afterward
stopCluster(cl)