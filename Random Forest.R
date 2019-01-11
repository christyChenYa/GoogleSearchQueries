
library(randomForest)
getwd()
setwd("/Users/Christy/Desktop/Data Mining ")
training<-read.csv("training.csv")
test=read.csv("test.csv")

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

training[,c('sig1_rk','sig2_rk','sig3_rk','sig4_rk','sig5_rk','sig6_rk','sig7_rk','sig8_rk'):=
           list(rank(sig1),rank(sig2),rank(sig3),rank(sig4),rank(sig5),rank(sig6),rank(sig7),rank(sig8)),by='query_id']  
training[,c('sig1_mean','sig2_mean','sig3_mean','sig4_mean','sig5_mean','sig6_mean','sig7_mean','sig8_mean'):=
           list(mean(sig1),mean(sig2),mean(sig3),mean(sig4),mean(sig5),mean(sig6),mean(sig7),mean(sig8)),by='query_id']

training[,c('sig1_max','sig2_max','sig3_max','sig4_max','sig5_max','sig6_max','sig7_max','sig8_max'):=
           list(max(sig1),max(sig2),max(sig3),max(sig4),max(sig5),max(sig6),max(sig7),max(sig8)),by='query_id']

######################### prepare for learning ######################################
train_feature<-training[,c(3:12,14:41)]
train_target<-training[,13]
################################################test##############
head(test,n = 20)
str(test)

#relevance = c(1:dim(test)[1])
#test$relevance = relevance

test<-test[,url_per_query:= .N,by='query_id']
test<-test[,url_median:=as.integer(median(url_id)),by='query_id']
test<-test[,url_distance:=log(1+abs(url_id-url_median))]

test<-test[,url_appearance:= .N,by='url_id']

test[,c('sig1_rk','sig2_rk','sig3_rk','sig4_rk','sig5_rk','sig6_rk','sig7_rk','sig8_rk'):=
       list(rank(sig1),rank(sig2),rank(sig3),rank(sig4),rank(sig5),rank(sig6),rank(sig7),rank(sig8)),by='query_id']  
test[,c('sig1_mean','sig2_mean','sig3_mean','sig4_mean','sig5_mean','sig6_mean','sig7_mean','sig8_mean'):=
       list(mean(sig1),mean(sig2),mean(sig3),mean(sig4),mean(sig5),mean(sig6),mean(sig7),mean(sig8)),by='query_id']

test[,c('sig1_max','sig2_max','sig3_max','sig4_max','sig5_max','sig6_max','sig7_max','sig8_max'):=
       list(max(sig1),max(sig2),max(sig3),max(sig4),max(sig5),max(sig6),max(sig7),max(sig8)),by='query_id']

######################### prepare for learning ######################################

#test_target=test[,13]
test_feature<-test[,c(3:40)]

##############RF#################
library(randomForest)

training$relevance<-factor(training$relevance)
set.seed(1)

rf.train1 = randomForest(relevance~., data=training, mtry=6,ntree=1000, importance=TRUE)

rf.pred = predict(rf.train1,newdata = test, type = "class")
summary(rf.pred)
rf.predf=ifelse(rf.pred==1,1,0)

write.table(rf.pred,"output1000.txt",sep="\n", row.names = FALSE, quote = FALSE, col.names = FALSE)


######## Analysis #################################
training.size <- dim(training)[1]
test = sample(1:training.size,training.size/10)
train=-test
training.test = training[test,]
training.train = training[-test,]

importance(rf.train1)

print(rf.train1)

importance(rf.train1)

varImpPlot (rf.train1)

x=table(rf.predf,training.test$relevance)
x
error_rate = (x[1,2]+x[2,1])/dim(training.test)[1]
error_rate
