##################Variable Selection#########################################
for(i in 1:dim(training)[1]){
  if(training[i,13]==0){
    training[i,13]=-1
  }
}
training.size <- dim(training)[1]
train = sample(1:training.size,training.size/10)
training.train = training[train,]
training.test = training[-train,]
x=model.matrix(relevance~., data=training.train[,3:13])[,-1]
y<-as.factor(training.train$relevance)
dat=data.frame(x=x,y=y)

seed<-1
lambda.scad <-c(1)
system.time(fit.fs <- svm.fs(x,y,fs.method = "scad", grid.search = "discrete", lambda.set=lambda.scad, maxIter = 10, cross.inner = 10, seed = seed))
print(fit.fs)

########Feature Selction - Recursive Feature Elimination###########
library("kernlab")
svm_rfe_single_step <- svm(y~x.query_length+x.is_homepage+x.sig1+x.sig2+x.sig6+x.sig7+x.sig8,data = dat[train,], kernel="linear", gamma=1, cost=0.1)
{
  ii<-rep(NA, ncol(x))
  i<-0
  while(any(is.na(ii))){
    not_elim_yet <- which(is.na(ii))
    fit<-ksvm(x[,not_elim_yet], y=y, scaled = FALSE, kernel = "rbfdot", kpar = "automatic", C = 1)
    sv_i<-alphaindex(fit)[[1]]
    w<-t(coef(fit[[1]]))%*%x[sv_i, not_elim_yet]
    to_elim <- not_elim_yet[head(order(w*w),1)]
    ii[to_elim] <- i 
    i < i+1
  }
  i -ii 
}

########rfe feature selection######
library("caret")
control <- rfeControl(functions=caretFuncs, method="cv", number=10)
results <- rfe(x, y, sizes=c(5), rfeControl=control , method="svmLinear")
print(results)
predictors(results)
