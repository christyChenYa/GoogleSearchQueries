library(data.table)

getwd()
setwd("/Users/Christy/Desktop/Data Mining ")
training<-read.csv("training.csv")
test=read.csv("test.csv")

######################### convert to data.table  ######################################
training<-data.table(training)
test<-data.table(test)
######################### check data  ################################################

head(training,n = 20)
str(training)

######################### Generate new features ######################################
training<-training[,url_per_query:= .N,by='query_id']
training<-training[,url_median:=as.integer(median(url_id)),by='query_id']
training<-training[,url_distance:=log(1+abs(url_id-url_median))]

training<-training[,url_appearance:= .N,by='url_id']
training<-training[,url_otherSuccess:= sum(relevance)-relevance,by='url_id']
training<-training[,url_otherSRatio:= url_otherSuccess/url_appearance]


training[,c('sig1_rk','sig2_rk','sig3_rk','sig4_rk','sig5_rk','sig6_rk','sig7_rk','sig8_rk'):=
           list(rank(sig1),rank(sig2),rank(sig3),rank(sig4),rank(sig5),rank(sig6),rank(sig7),rank(sig8)),by='query_id']  
training[,c('sig1_mean','sig2_mean','sig3_mean','sig4_mean','sig5_mean','sig6_mean','sig7_mean','sig8_mean'):=
           list(mean(sig1),mean(sig2),mean(sig3),mean(sig4),mean(sig5),mean(sig6),mean(sig7),mean(sig8)),by='query_id']

training[,c('sig1_max','sig2_max','sig3_max','sig4_max','sig5_max','sig6_max','sig7_max','sig8_max'):=
           list(max(sig1),max(sig2),max(sig3),max(sig4),max(sig5),max(sig6),max(sig7),max(sig8)),by='query_id']

######################### prepare for learning ######################################
train_feature<-training[,c(3:12,14:43)]
train_target<-training[,13]

###############ADA BOOSTING###########################
library(adabag)
library(rpart)
training$relevance<-factor(training$relevance)
set.seed(1)
k=10
folds=sample(1:k,nrow(training),replace=TRUE)
for(i in 1:k){
  train.adaboost=boosting(relevance~.,data=training[folds!=i,],mfinal=100, coeflearn="Breiman",control=rpart.control(maxdepth=30),boos=TRUE)
  train.pred <- predict.boosting(train.adaboost,newdata=training[folds==i,])
  print(train.pred$confusion)
  print(train.pred$error)
  errorevol(train.adaboost,newdata=training[folds!=i,])->evol.train
  errorevol(train.adaboost,newdata=training[folds==i,])->evol.test
  print(plot.errorevol(evol.test,evol.train))
}

evol.test

#######################xgb########################################
library(xgboost)
#test_set=sample(train_feature[1:dim(train_feature),1:dim(train_feature)/10])
#  dtest <- xgb.DMatrix(data.matrix(train_feature[test_set,]),
#                      label = train_target[,relevance][test_set])
dtrain <- xgb.DMatrix(data.matrix(train_feature),
                      label = train_target[,relevance])

watchlist <- list(eval = dtrain)#, train = dtrain)

param <- list(max_depth = 8, eta = 0.1, silent = 1, 
              objective = "binary:logistic")
bst <- xgb.cv(param, dtrain, nrounds = 500, watchlist,nfold=10)

#############test########################################
testM<-data.matrix(test)
dtest<-xgb.DMatrix(testM)
bst.pred <- predict(bst, newdata = dtest, type = "class")
bst.predf=ifelse(bst.pred<=0.5,1,0)

test<-test[,]

#############Plot & Summary################################
summary(bst$evaluation_log)
importance_matrix <- xgb.importance(colnames(train_feature), model = bst)

xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")

###############Shrinkage Plot######################
for(i in 1:length(lambda)){
  boost.hitters <-gbm(Salary ~., data = Hitters.train,distribution = "gaussian", n.trees = 1000, shrinkage = lambda[i])
  pred.train <-predict(boost.hitters, Hitters.train, n.tree=1000)
  train.err[i]<-mean((pred.train-Hitters.train$Salary)^2)
}
plot(lambda,train.err,type = "b", xlab = "Shrinkage values", ylab = "Training MSE")


train.pred<-predict(bst,newdata = dtrain,type="class")
train.predf=ifelse(train.pred<=0.5,0,1)