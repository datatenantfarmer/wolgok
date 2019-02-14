###  Linear model selection and regularization ###

### Best subset selection ###
library(ISLR)

fix(Hitters)
dim(Hitters)
# omitting na
Hitters<-na.omit(Hitters)

library(leaps)
model<-regsubsets(Salary~., Hitters)
# row : # of parameter in model 
# * : included in model
summary(model)

# by using "nvmax" options, we can adjust # of parameters in model
model2<-regsubsets(Salary~., Hitters, nvmax=9)
model3<-regsubsets(Salary~., Hitters, nvmax=19)
summary.model3<-summary(model3)
# goodness of fit indeces
plot(summary.model3$rss, type="l")
plot(summary.model3$adjr2, type="l")
which.max(summary.model3$adjr2)
which.min(summary.model3$cp)
which.min(summary.model3$bic)

# excess regression parameter 
# 6 parameter model
coef(model3, 6)

### forward selection and backward elimation ###
model.forward<-regsubsets(Salary~., Hitters, nvmax=19, method="forward")
summary(model.forward)
model.backward<-regsubsets(Salary~., Hitters, nvmax=19, method="backward")
summary(model.backward)

### CV to select best model ###
index<-sample(c(0,1), dim(Hitters)[1], replace = T, prob = c(.3,.7))
model.train<-regsubsets(Salary~., Hitters[index==1,], nvmax=19)
test.mat<-model.matrix(Salary~., Hitters[index==0,])

val.error<-rep(NA, 19)
for (i in 1:19){
  coefi<-coef(model.train, i)
  pred<-test.mat[, names(coefi)]%*%coefi
  val.error[i]<-mean((Hitters$Salary[index==0]-pred)^2)
}

plot(val.error, type="l")
coef(model.train, 10)


### Ridge and Lasso regression ###
install.packages("glmnet")
library(glmnet)
# Ridge
grid<-10^seq(10, -2, length.out = 100)
model.ridge<-glmnet(model.matrix(Salary~., Hitters)[,-1], Hitters$Salary, alpha=0,lambda=grid)
# 'glmnet' standardize data automatically 
plot(model.ridge)
cv.ridge<-cv.glmnet(model.matrix(Salary~., Hitters)[index==1,-1], Hitters$Salary[index==1], alpha=0)
cv.ridge$lambda.min
coef(model.ridge)[,50]

ridge.coef<-predict(model.ridge, type="coefficients", s=cv.ridge$lambda.min)
ridge.coef

# Lasso
model.lasso<-glmnet(model.matrix(Salary~.,Hitters)[index==1,-1], Hitters$Salary[index==1], alpha=1, lambda=grid)
plot(model.lasso)

# select best tuning parameter
cv.lasso<-cv.glmnet(model.matrix(Salary~., Hitters)[index==1,-1], Hitters$Salary[index==1], alpha=1)
# MSE Vs. log(lambda)
plot(cv.lasso)
cv.lasso$lambda.min
lasso.pred<-predict(model.lasso, s=cv.lasso$lambda.min, newx=model.matrix(Salary~., Hitters)[index==0, -1])
mean((lasso.pred-Hitters$Salary[index==0])^2)

# Lasso coefficient
lasso.coef<-predict(model.lasso, type="coefficients", s=cv.lasso$lambda.min)
lasso.coef

### PCR and PLS regression ###

### PCR ###
install.packages("pls")
library(pls)
model.pcr<-pcr(Salary~., data=Hitters, scale=T, validation="CV")
summary(model.pcr)

validationplot(model.pcr, val.type = "RMSEP")

model.pcr<-pcr(Salary~., data=Hitters, subset=(index==1), scale=T, validation="CV")
validationplot(model.pcr)

pcr.pred<-predict(model.pcr, model.matrix(Salary~.,Hitters)[index==0,-1], ncomp=5)
mean((pcr.pred-Hitters$Salary[index==0])^2)

pcr.model<-pcr(Salary~., data=Hitters, scale=T, ncomp=5)
summary(pcr.model)

### PLS ###
pls.model<-plsr(Salary~., data=Hitters, subset=(index==1), scale=T, validation="CV")
summary(pls.model)
validationplot(pls.model, val.type="RMSEP")

pls.pred<-predict(pls.model, model.matrix(Salary~., Hitters)[index==0,-1], ncomp=2)
mean((pls.pred-Hitters$Salary[index==0])^2)

pls.model<-plsr(Salary~., data=Hitters, scale=T, ncomp=2)
summary(pls.model)
