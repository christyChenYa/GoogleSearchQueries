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


############lasso##################
library(glmnet)
set.seed(1)
test = sample(1:dim(training)[1],dim(training)[1]/10)
train=(-test)
train=training[train,]
test=training[test,]
train.kfoldx=model.matrix(relevance~., data=train)
train.kfoldy=train$relevance
test.kfoldx=model.matrix(relevance~.,data=test)
test.kfoldy=test$relevance
grid <- 10 ^ seq(4, -2, length = 100)
lasso.mod <- glmnet(train.kfoldx,train.kfoldy,family="binomial",alpha=1,lambda=grid)
cv.lasso <- cv.glmnet(train.kfoldx,train.kfoldy,family="binomial",nfolds = 10,alpha=1,lambda=grid,type.measure = "class")
bestlam.lasso <- cv.lasso$lambda.min
cv.lasso$lambda
lasso.coef=predict(lasso.mod,type="coefficients", s=bestlam.lasso)
lasso.coef
lasso.pred=predict(lasso.mod,s=bestlam.lasso, newx=test.kfoldx, type = "class")
lasso.predf=ifelse(lasso.pred==0,0,1)
x=table(lasso.predf,test$relevance)
x
error.rate=(x[1,2]+x[2,1])/dim(test)[1]
error.rate

plot(cv.lasso$lambda,cv.lasso$cvm)

#####################best subset################################
library(leaps)
regfit.mix=regsubsets(relevance~., data=training, nvmax=26)
reg.summary=summary(regfit.mix)
names(reg.summary)
which.min(reg.summary$bic)
coef(regfit.mix,14)
val.error=rep(NA,26)
pred=c(1:14)
for(i in 1:14){
  coefi=coef(regfit.mix,id=i)
  pred[i]=testx[,names(coefi)]%*%coefi
}
for(i in 2:7){
  pred[i]=pred[i]+pred[i-1]
}
x=table(pred,testy)
x