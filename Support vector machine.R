library(e1071)

### Hard / Soft margin classifier ###
x<-matrix(rnorm(20*2), nc=2)
y<-c(rep(-1, 10), rep(1,10))
x[y==1,]<-x[y==1,]+1
plot(x, col=(3-y))
data<-data.frame(x=x, y=as.factor(y))
# cost means tuning parameter in soft margin classifier
model.hm<-svm(y~., data=data, kernel="linear", cost=10, scale=FALSE)
# Vidualization of classification
plot(model.hm, data)
# x in plot : support vectors
# index of support vectors
model.hm$index
summary(model.hm)

model.sm<-svm(y~., data=data, kernel="linear", cost=0.1, scale=FALSE)
plot(model.sm, data)
# with smaller C, margin becomes wide : # of support vectors increases

# 'tune()' : perform cross validation
tune.out<-tune(svm, y~., data=data, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 100)))
# Suggest best tuning parameter based on cross validation
summary(tune.out)
# obtain best model 
best.svm<-tune.out$best.model
summary(best.svm)
plot(best.svm, data)

# generate test set 
x.test<-matrix(rnorm(20*2), nc=2)
y.test<-sample(c(-1, 1), 20, replace = T)
x.test[y.test==1,]<-x.test[y.test==1,]+1
test.set<-data.frame(x=x.test,y=as.factor(y.test))
pred<-predict(best.svm, test.set)
# confusion matrix
table(pred=pred, actual=test.set$y)

x[y==1,]<-x[y==1, ]+0.5
plot(x, col=(y+5)/2)

data<-data.frame(x=x, y=as.factor(y))
tune.out2<-tune(svm, y~., data=data, kernel="linear", ranges=list(cost=seq(1e-2, 1e+2, length.out = 100)))
best.svm2<-tune.out2$best.model

summary(best.svm2)
plot(best.svm2, data)

### Support vector machine ###
x<-matrix(rnorm(200*2), nc=2)
x[1:100,]<-x[1:100,]+2
x[101:150,]<-x[101:150,]-2
y<-c(rep(1,150), rep(2, 50))
data<-data.frame(x=x, y=as.factor(y))
# Hard to classify with linear kernel
plot(x, col=y)

tr<-sample(200, 100)
tune.out3<-tune(svm, y~., data=data[tr,], kernel="radial", decision.values=T,compprob=T,
                ranges=list(cost=seq(1e-2, 1e+2, length.out = 10), 
                                     gamma=seq(1e-1, 1e+1, length.out = 10)))
summary(tune.out3)
best.svm3<-tune.out3$best.model
summary(best.svm3)
plot(best.svm3, data[tr,])

# train error
table(true=data[tr,"y"], pred=predict(best.svm3, newdata = data[tr,]))
# Test error
table(true=data[-tr, "y"], pred=predict(best.svm3, newdata = data[-tr,]))

### SVM with multiple classes ###
library(ISLR)
names(Khan)

data<-data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
tune.out4<-tune(svm, y~., data=data, kernel="linear", ranges=list(cost=seq(1e-2, 1e+2, length.out = 3)))
best.svm4<-tune.out4$best.model
summary(best.svm4)
pred<-predict(best.svm4, newdata = Khan$xtrain)
table(pred=pred, actual=Khan$ytrain)
pred_test<-predict(best.svm4, newdata=Khan$xtest)
table(pred=pred_test, actual=Khan$ytest)
