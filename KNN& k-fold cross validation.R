### KNN ###

library(class)
dat<-read.table("germandata.txt", header=T)
dat
dat$numcredits = factor(dat$numcredits)
dat$residence = factor(dat$residence)
dat$residpeople = factor(dat$residpeople)
dat$y = ifelse(dat$y=="good", 1, 0)
dat$y = factor(dat$y)

sum(is.na(dat))

summary(dat)
str(dat)
barplot(table(dat$y))

c(2,5,8,13,21)
dat2<-dat[,c(2,5,8,13,21)]
dat2

dat3<-data.frame(scale(dat2[,-5]), dat2[,5])
dat3

model<-knn(train=dat3[,-5], test=dat3[,-5], cl=dat3[,5],k=5)
# confusion matrix
conmat<-table(dat3[,5], model, dnn=c("Actual", "Predicted"))
conmat
# model Accuracy
Acc<-sum(diag(conmat))/sum(conmat)
# Error rate
Er<-1-Acc
margin<-apply(conmat,1,sum)
# Specificity
conmat[1,1]/margin[1]
# Sensitivity
conmat[2,2]/margin[2]

# train set & test set
n<-dim(dat3)[1]
n
id<-sample(1:2, n ,prob=c(0.7,0.3), replace=T)
index_train<-which(id==1)

# validate model
model_val<-knn(train=dat3[index_train,-5], test=dat3[-index_train,-5],cl=dat3[index_train,5], k=5)

conmat2<-table(dat3[-index_train,5], model_val, dnn=c("Actual", "Predicted"))

performance_class<-function(mat){
  Acc<-sum(diag(mat))/sum(mat)
  Er<-1-Acc
  margin<-apply(mat,1,sum)
  Spec<-mat[1,1]/margin[1]
  Sen<-mat[2,2]/margin[2]
  list(Acc=Acc, Er=Er, Spec=Spec, Sen=Sen)
}

performance_class(conmat)
performance_class(conmat2)

# G fold cross validation
kfold_knn<-function(g, data, col.label, col.x, nei){
  n<-dim(data)[1]
  id<-sample(1:g, n, replace=T)
  cv<-matrix(rep(0, g*4),g,4)
  for (i in 1:g){
    model<-knn(train=data[which(id==i),col.x], test=data[-which(id==i), col.x], cl=data[which(id==i),col.label], k=nei)
    conmat<-table(data[-which(id==i),col.label], model, dnn=c("Actual", "Predicted"))
    Acc<-sum(diag(conmat))/sum(conmat)
    Er<-1-Acc
    margin<-apply(conmat,1,sum)
    Spec<-conmat[1,1]/margin[1]
    Sen<-conmat[2,2]/margin[2]
    cv[i,]<-c(Acc, Er, Spec, Sen)
    colnames(cv)<-c("Accuracy","Error","Specificity","Sensitivity")
    rownames(cv)<-1:g
  }
  return(apply(cv,2,mean))
}

kfold_knn(5, dat3, 5, 1:4, 5)

