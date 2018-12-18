library(e1071)
library(penalizedSVM)
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

######################### prepare for learning ######################################
train_feature<-training[,c(3:12,14:25,27)]
train_target<-training[,13]

for(i in dim(training)[1]){
  if(training[i,13]==0){
    training[i,13]=-1
  }
}
#training$relevance=factor(training$relevance)
x=model.matrix(relevance~., data=training)[,-1]
y<-as.factor(training$relevance)
dat=data.frame(x=x,y=y)
train=sample(1:dim(training)[1],dim(training)[1]/2)
test=-train
svm.fit <- svm(y~.,data=dat[train,], kernel="linear", gamma=1,cost=0.1,scale=TRUE)
ypred=predict(svm.fit,newdata = dat[test,], scale=TRUE)
x=table(true=dat[test,27],pred=ypred)
x
(x[1,2]+x[2,1])/dim(dat[test,])[1]


####################Cross Validation##########################################
training.size <- dim(training)[1]
train = sample(1:training.size,training.size/2)
training.train = training[train,]
training.test = training[-train,]

error.rate=c(1:10)
k=10
folds=sample(1:k,nrow(training.train),replace=TRUE)

for(i in 1:k){
  set.seed(1)
  x=model.matrix(relevance~., data=training.train)[,-1]
  y<-as.factor(training$relevance)
  dat=data.frame(x=x,y=y)
  train.kfolds=dat[folds!=i,]
  test.kfolds=dat[folds==i,]
  for(j in 1:10){
    fit <-svm(y~.,data = train.kfolds, kernel="linear", gamma=1, cost=0.1, scale = TRUE)
    
  }

  ypred=predict(fit,test.kfolds, type = "response")
  h=table(predict=ypred,truth=test.kfolds[,"y"])
  error.rate [i] = (h[1,2]+h[2,1])/dim(test.kfolds)[1]
}

error.rate
bestind = which.min(error.rate)
bestind

set.seed(1)
fit.best <- svmy~x.query_length+x.is_homepage+x.sig1+x.sig2+x.sig6+x.sig7+x.sig8, data = , kernel="linear", gamma=1, cost=0.1, scale = TRUE()

set.seed(1)

########################################Tune#################################
set.seed(1)
tune.out=tune(svm, y~.,data = dat[train, ],kernel="linear",ranges = list(cost=c(0.1,1,10)), gamma=c(0.5,1,2))

##########just use CV to find important predictors 
