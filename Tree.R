#treelab
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

library(tree)
#error.rate=c(1:10)
#k=10
#folds=sample(1:k,nrow(training),replace=TRUE)
High=ifelse(training$relevance==0,"N","Y")
training=data.frame(training,High) 	
training.size <- dim(training)[1]
test = sample(1:training.size,training.size/10)
train=-test
training.test = training[test,]
training.train = training[-test,]
#for(i in 1:k){
  set.seed(1) 
  #train=training[folds!=i,]
  #test=training[folds==i,]
  #High.test=High[folds==i]
  
  tree.train=tree(High~.,data=training.test)
  summary(tree.train)
  
  tree.pred=predict(tree.train,training.test,type="class")
  x=table(tree.pred,High.trainingtest)
  error.rate=(x[1,2]+x[2,1])/dim(test)[1]
  
  
#}
mean(error.rate)
bestind = which.min(error.rate)
bestind
set.seed(1)
tree.best=tree(High~., data=training[folds==bestind,])
summary(tree.best)


###################CV PLOT########################
cv.relevance <- cv.tree(rf.train1)
plot(cv.relevance$size, cv.relevance$dev, type = "b")
tree.min <- which.min(cv.relevance)
points(cv.relevance$size[tree.min], cv.relevance$dev[tree.min], col="red", cex =2, pch=20)
