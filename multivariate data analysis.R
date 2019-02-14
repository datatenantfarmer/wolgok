# GSP_raw
setwd("/Users/chanheelee/Desktop/lecture/Multivariate Data Analysis/raw data")
gsp<-read.table("GSP_RAW.txt", header=T)
dim(gsp)
for (i in 1:13){
  gsp[,i]<-gsp[,i]/sd(gsp[,i])
}

pca_gsp<-prcomp(gsp, scale. = T)
loadings_gsp<-pca_gsp$rotation%*%diag(pca_gsp$sdev)
loadings_gsp

# spree plot
plot(pca_gsp$sdev^2)

pca_gsp$sdev^2
new<-pca_gsp$x[,1:2]

# horn's procedure
# bootstrapping
boot<-function(df, n){
  nc<-dim(df)[2]
  new_df<-matrix(rep(0, n*nc), nrow=n, ncol=nc)
  for (i in 1:nc){
    new_df[,i]<-df[sample(dim(df)[1], n, replace = F),i]
  }
  return(new_df)
}
boot_gsp<-boot(gsp, 20)
boot_gsp
pca_boot<-prcomp(boot_gsp, scale. = T)
pca_boot
plot(pca_gsp$sdev^2, type="l")
lines(pca_boot$sdev^2, type="l", col="red")

#GSP_SHARE
gsp2<-read.csv("gsp_share.csv", header=T)
gsp2<-gsp2[-c(2,50),-1]
gsp2
pca_gsp2<-prcomp(gsp2, scale. = T)
pca_gsp2
pca_gsp2$sdev^2
loadings_gsp2<-pca_gsp2$rotation%*%diag(pca_gsp2$sdev)
loadings_gsp2
plot(pca_gsp2$x)
plot(pca_gsp2$x)
plot(loadings_gsp2)
abline(h=0, v=0)
plot(pca_gsp2$sdev^2, type="l")
points(pca_gsp2$sdev^2)
pca_gsp2$sdev^2
boot_gsp2<-boot(gsp2, 20)
pca_boot2<-prcomp(boot_gsp2, scale. = T)
lines(pca_boot2$sdev^2, col="red")
points(pca_boot2$sdev^2, col="red")



# PCA 4.2
ex1<-read.table("PCA_EXAMPLE.txt", header=F)
ex1
pca1<-prcomp(ex1, scale. = T)
pca1
cor(ex1)
pca1$rotation[,1]*pca1$sdev[1]
# Yields principle component loadings
loading<-pca1$rotation%*%diag(pca1$sdev)


# PCA Governmennt - Using linear aljebra
gover<-read.csv("gover.csv", header=T)
gover
for (i in 1:6){
  for (j in 1:6){
    gover[i,j]<-gover[j,i]
  }
}
gover
u_mat<-eigen(gover)$vectors
lam<-eigen(gover)$values
u_mat
lam
sum(lam[1:2])/6
std<-sqrt(lam)
u_mat%*%diag(std)

# Iris data PCA
iris<-read.csv("iris.csv", header=T)
iris
# correlation matrix
cor(iris[,-1])
pca_iris<-prcomp(iris[,-1], scale. = T)
pca_iris

loadings_iris<-pca_iris$rotation%*%diag(pca_iris$sdev)
loadings_iris
plot(pca_iris$sdev^2, type="l")
points(pca_iris$sdev^2)

boot_iris<-boot(iris[,-1], 100)
pca_boot3<-prcomp(boot_iris, scale. = T)
lines(pca_boot3$sdev^2, type="l", col="red")
points(pca_boot3$sdev^2, col="red")
pca_iris$sdev^2

spe1<-which(iris$Col1==1)
spe2<-which(iris$Col1==2)
spe3<-which(iris$Col1==3)

plot(pca_iris$x[spe1,1]~pca_iris$x[spe1,2])
plot(pca_iris$x[spe2,1]~pca_iris$x[spe2,2])
plot(pca_iris$x[spe3,1]~pca_iris$x[spe3,2])

# Cognition data
co<-read.csv("cognition.csv", header=T)
mat<-co[,-1]
mat
for (i in 1:dim(mat)[2]){
  mat[,i]<-as.numeric(mat[,i])
}

bart.test<-function(df){
  cormat<-cor(df)
  val<- -(dim(df)[1]-1-(2*dim(df)[2]+5)/6)*log(det(cormat))
  dgf<- (dim(df)[2]^2-dim(df)[2])/2
  pval<-1-pchisq(val, dgf)
  list(val, dgf, pval)
}

bart.test(mat)

pca_co<-prcomp(mat, scale. = T)
plot(pca_co$sdev^2, type="l")
points(pca_co$sdev^2)
dim(co)
boot_co<-boot(mat, 100)
pca_boot4<-prcomp(boot_co, scale.=T)
lines(pca_boot4$sdev^2, col="red")

# PCA : Vocations
voca<-read.csv("vocations.csv", header=T)
voca
for (i in 1:dim(voca)[1]){
  for (j in 1:dim(voca)[2]){
    voca[i,j]<-voca[j,i]
  }
}
voca
voca_u<-eigen(voca)$vectors
voca_lam<-eigen(voca)$values
voca_lam
plot(voca_lam, type="l")
points(voca_lam)
loadings_voca<-voca_u%*%diag(sqrt(voca_lam))
loadings_voca
redim<-voca_u[,1:7]
redim
a<-loadings_voca[,1:7]
plot(a[,2]~a[,1], xlim=c(-1, 0.5), col=1)
abline(v=0, h=0)
points(a[,3]~a[,1], col=2)
points(a[,4]~a[,1], col=3)
points(a[,5]~a[,1], col=4)
points(a[,6]~a[,1], col=5)
points(a[,7]~a[,1], col=6)

plot(a[,3]~a[,2], col=1)
points(a[,4]~a[,2], col=2)
points(a[,5]~a[,2], col=3)
points(a[,6]~a[,2], col=4)
points(a[,7]~a[,2], col=5)

# Records
rec<-read.csv("records.csv", header=T)
rec<-rec[,-c(1,10)]
rec
pca_run<-prcomp(rec, scale.=T)
a<-pca_run$sdev^2
sum(a[1])/8
plot(a, type="l")
points(a)
boot_run<-boot(rec, 25)
pca_boot5<-prcomp(boot_run, scale.=T)
b<-pca_boot5$sdev^2
lines(b, col="red")
points(b, col="red")
a
summary(pca_run)

# Drug
drug<-read.csv("drug.csv", header=T)
drug
# form correlation matrix
for (i in 1:dim(drug)[1]){
  for (j in 1:dim(drug)[2]){
    drug[i,j]<-drug[j,i]
  }
}

u<-eigen(drug)$vectors
val<-eigen(drug)$values
plot(val, type="l")
points(val)
# loadings
loadings_drug<-u%*%diag(sqrt(val))
val
bart.test2<-function(cor,n,p){
  cor<-as.matrix(cor)
  val<--((n-1)-(2*p+5)/6)*log(det(cor))
  df<-(p^2-p)/2
  pval<-1-pchisq(val, df)
  list(val, df, pval)
}
drug
bart.test2(drug, 1634, 13)

loadings_drug
plot(loadings_drug)
loadings_drug
u

# Foreign conflict
con<-read.csv("conflict.csv", header=T)
con
for (i in 1:dim(con)[1]){
  for (j in 1:dim(con)[2]){
    con[i,j]<-con[j,i]
  }
}

rotation<-eigen(con)$vectors
val<-eigen(con)$values
plot(val, type="l")
points(val)
rotation[,1:4]%*%diag(sqrt(val[1:4]))

# Ch5. EFA
# Luxury car data
car<-read.csv("car.csv", header=T)
car
for (i in 1:dim(car)[1]){
  for (j in 1:dim(car)[2]){
    car[i,j]<-car[j,i]
  }
}
car

pca_car<-prcomp(car, scale.=T)
plot(pca_car$sdev^2)
# Indicate 2 factor
str(car)
car<-as.matrix(car)
str(car)

efa_car<-factanal(covmat=car, factors = 2, method="mle", rotation = "varimax")


# Test data
test<-read.csv("tests.csv", header=T)
test<-test[4:8, 4:8]
test
for (i in 1:dim(test)[1]){
  for (j in 1:dim(test)[2]){
    test[i,j]<-test[j,i]
  }
}
test
test<-as.matrix(test)

pca_test<-prcomp(test, scale. = T)
plot(pca_test$sdev^2)
efa_test<-factanal(covmat=test, factors=2, method="mle", rotation = "varimax")

# Pain reliver data
pain<-read.csv("pain.csv", header=T)
pca_pain<-prcomp(pain, scale.=T)
plot(pca_pain$sdev^2)

efa_pain1<-factanal(pain, factors = 2, scores="regression", rotation = "none")
efa_pain1
plot(efa_pain1$loadings)
abline(h=0, v=0)

efa_pain2<-factanal(pain, factors = 2, scores="regression", rotation = "varimax")
efa_pain2
plot(efa_pain2$loadings)
abline(h=0, v=0)
efa_pain2$scores

# RTE cereals data
rte<-read.csv("rte.csv", header=T)
rte

pca_rte<-prcomp(rte[,-c(1,2)], scale. = T)
pca_rte$sdev^2
plot(pca_rte$sdev^2)

efa_rte1<-factanal(rte[,-c(1,2)], factors = 4, scores="regression", rotation="none")
efa_rte1
# communality estimates
1-efa_rte1$uniquenesses
efa_rte2<-factanal(rte[,-c(1,2)], factors=4, scores="regression", rotation = "varimax")
a<-efa_rte2$scores
plot(a[,1]~a[,2])
plot(a[,3]~a[,4])

# ex5.1
test<-read.csv("tests.csv", header=T)
test
for (i in 1:dim(test)[1]){
  for (j in 1:dim(test)[2]){
    test[i,j]<-test[j,i]
  }
}
test

pca_test<-prcomp(test, scale.=T)
plot(pca_test$sdev^2, type="l")
points(pca_test$sdev^2)

test<-as.matrix(test)
efa_test<-factanal(covmat = test, factors = 3, rotation="varimax")
efa_test

# ex5.2
rte<-read.csv("rte.csv", header=T)
rte
pca_rte<-prcomp(rte[,-c(1,2)], scale. = T)
plot(pca_rte$sdev^2, type="l")
points(pca_rte$sdev^2)
pca_rte$sdev^2

efa_rte3<-factanal(rte[,-c(1,2)], factors=5,rotation="varimax", scores = "regression")
efa_rte3

# ex5.3
drink<-read.csv("drink.csv", header=T)
drink

for (i in 1:dim(drink)[1]){
  for (j in 1:dim(drink)[2]){
    drink[i,j]<-drink[j,i]
  }
}

drink<-as.matrix(drink)
drink

pca_drink<-prcomp(drink, scale.=T)
plot(pca_drink$sdev^2, type="l")
points(pca_drink$sdev^2)
pca_drink$sdev^2

efa_drink<-factanal(covmat=drink, factors=2, rotation="varimax")
efa_drink
1-efa_drink$uniquenesses

# ex5.4
var<-read.csv("six_variables.csv", header=T)
var
pca_var<-prcomp(var, scale. = T)
plot(pca_var$sdev^2, type="l")
points(pca_var$sdev^2)
pca_var$sdev^2

efa_var<-factanal(var, factors=3, scores="regression", rotation="varimax")
efa_var
sum(1-efa_var$uniquenesses)/6
efa_var$scores

# ex5.5
luc<-read.csv("car.csv", header=T)
luc
for (i in 1:dim(luc)[1]){
  for (j in 1:dim(luc)[2]){
    luc[i,j]<-luc[j,i]
  }
}
luc
luc<-as.matrix(luc)
pca_car2<-prcomp(luc, scale.=T)
plot(pca_car2$sdev^2, type="l")
points(pca_car2$sdev^2)
pca_car2$sdev^2

efa_luc<-factanal(covmat=luc, factors = 3, rotation="none")
efa_luc
sum(1-efa_luc$uniquenesses)/9

# ex5.6
food<-read.csv("food_a.csv", header=T)
food

pca_food<-prcomp(food[,-1], scale.=T)
plot(pca_food$sdev^2, type="l")
points(pca_food$sdev^2)
pca_food$sdev^2
dim(food)
boot_food<-boot(food,100)
pca_boot6<-prcomp(boot_food, scale. = T)
lines(pca_boot6$sdev^2, col="red")


efa_food<-factanal(food[,-1], factors=3, scores="regression", rotation="varimax")
efa_food
sum(1-efa_food$uniquenesses)/10
loadings<-efa_food$loadings[1:10,]
loadings
S<-matrix(rep(0, 3*10), nrow=10, ncol=3)
for (i in 1:10){
  for (j in 1:3){
  if (loadings[i,j]>=0.59){
   S[i,j]<-1
  }else{
    S[i,j]<-0
  }
  }
}
S

food_val<-read.csv("food_b.csv", header=T)
food_val
pca_val<-prcomp(food_val[,-1], scale.=T)
plot(pca_val$sdev^2, type="l")
points(pca_val$sdev^2)
pca_val$sdev^2

efa_food_val<-factanal(food_val[,-1], factors=3, scores="regression", rotation="varimax")
S1<-matrix(rep(0, 3*10), nrow=10, ncol=3)
S1
loadings_val<-efa_food_val$loadings[1:10,]
loadings_val
for (i in 1:10){
  for (j in 1:3){
    if (loadings_val[i,j]>=0.59){
      S1[i,j]<-1
    }else{
      S1[i,j]<-0
    }
  }
}
S; S1

# ex5.7
mba<-read.csv("mba_car.csv", header=T)
mba
for (i in 1:dim(mba)[2]){
  mba[,i]<-as.numeric(mba[,i])
}
str(mba)
pca_mba<-prcomp(mba[,-c(1,2)], scale.=T)
plot(pca_mba$sdev^2, type="l")
points(pca_mba$sdev^2)
pca_mba$sdev^2
dim(mba)

efa_mba<-factanal(mba[,-c(1,2)], factors=3, scores="regression", rotation="varimax")
efa_mba
score<-efa_mba$scores
sam<-cbind(mba[,2], score)

new<-matrix(rep(0, 10*4), nrow=10, ncol=4)
for (i in 1:10){
  k<-which(sam[,1]==i)
  new[i,]<-apply(sam[k,], 2, mean)
}
new
plot(new[,2]~new[,1])
plot(new[,3]~new[,1])
plot(new[,4]~new[,1])

# ex5.8
emo<-read.csv("emotions.csv", header=T)
emo
for (i in 1:dim(emo)[1]){
  for (j in 1:dim(emo)[2]){
    emo[i,j]<-emo[j,i]
  }
}
emo
pca_emo<-prcomp(emo, scale. = T)
plot(pca_emo$sdev^2, type="l")
points(pca_emo$sdev^2)
pca_emo$sdev^2
emo<-as.matrix(emo)
pca_emo$rotation%*%diag(pca_emo$sdev)[,1:2]
a<-pca_emo$sdev^2

efa_emo<-factanal(covmat=emo, factors=2, rotation="varimax")
efa_emo

# Confirmatory factor analysis
install.packages("sem")
library(sem)
install.packages("lavaan")
library(lavaan)

efa_test<-factanal(covmat = test[4:9, 4:9], factors=2, rotation="varimax")
efa_test
new_test<-test[4:9, 4:9]
lower<-'1.000
0.722 1.000
0.714 0.685 1.000
0.203 0.246 0.170 1.000
0.095 0.181 0.113 0.585 1.000'
# lavaan with covariance matrix
test<-read.csv("tests.csv", header=T)
test<-test[4:9,4:9]
covmat<-getCov(lower, names = c("Col4", "Col5","Col6", "Col7", "Col8"))
model<-'factor1=~Col4+Col5+Col6
factor2=~Col7+Col8'
model3<-'factor=~Col4+Col5+Col6+Col7+Col8'
fit1<-cfa(model, sample.cov = covmat, sample.nobs = 145)
summary(fit1, fit.measures=T)
fit1_1<-cfa(model3, sample.cov = covmat, sample.nobs = 145)
summary(fit1_1, fit.measures=T)

# wine status data
wine<-read.csv("wine_status.csv", header=T)
temp<-'2.014611
2.147575 3.912916
1.338691 2.022501 1.933957
1.663355 2.148159 1.524839 3.012858
1.845412 2.888662 1.892168 1.712741 3.635885'
covmat<-getCov(temp, names=c("Col1","Col2","Col3","Col4","Col5"))
model1<-'factor1=~Col1+Col2+Col3+Col4+Col5'
fit2<-cfa(model1, sample.cov = covmat, sample.nobs = 59)
summary(fit2, fit.measures=T, standardized=T)

efa<-factanal(wine, factors=1, scores="regression", rotation="varimax")
efa
wine
fit2_1<-cfa(model1, data=wine, group.equal=c("loadings"))
summary(fit2_1, fit.measures=T)


# Store data
store<-read.csv("store.csv", header=T)
store
store<-store[c(1,2,5,6,9,10),c(1,2,5,6,9,10)+2]
store
rownames(store)<-c("AL","PL","AD","PD","AS","PS")
store
colnames(store)<-c("AL","PL","AD","PD","AS","PS")
store
temp<-'1.000
0.638 1.000
0.776 0.600 1.000
0.561 0.713 0.635 1.000
0.676 0.539 0.739 0.527 1.000
0.522 0.720 0.559 0.698 0.589 1'

cov_store<-getCov(temp, names=c("AL","PL","AD","PD","AS","PS"))
model4<-'factor1 =~ AL+AD+AS
factor2 =~ PL+PD+PS'
cfa_store<-cfa(model4, sample.cov = cov_store, sample.nobs = 250)
summary(cfa_store, fit.measures=T, standardized=T)

# MMTM
cov_store
model5<-'factor1 =~ AL+AD+AS
factor2 =~ PL+PD+PS
factor3 =~ AL+PL
factor4 =~ AD+PD
factor5 =~ AS+PS'
dmat<-diag(sqrt(c(0.274, 0.173, 0.345,0.258,0.295,0.312)))
cov_store2<-dmat%*%cov_store%*%dmat
rownames(cov_store2)<-c("AL","PL","AD","PD","AS","PS")
colnames(cov_store2)<-c("AL","PL","AD","PD","AS","PS")
cfa_store2<-cfa(model5, sample.cov = cov_store2, sample.nobs = 250)

model6<-'factor1 =~ AL+AD+AS
factor2 =~ PL+PD+PS
AL~~PL
AD~~PD
AS~~PS'
cfa_store3<-cfa(model6, sample.cov = cov_store, sample.nobs = 250)
summary(cfa_store3, fit.measures=T, standardized=T)

# ex6.1
lam<-c(0.725,0.65,0.58,0.605,0.81)
theta<-c(.474,.578,.664,.634,.344)
sum(lam^2)/(sum(lam^2)+sum(theta^2))

# ex6.2
.69*.84*.36

# ex6.3
drink<-read.csv("drink.csv", header=T)
drink
mat<-'
1.000
0.886 1.000
0.649 0.597  1.000
0.588 0.621  0.649  1.000
0.067 0.034 -0.080 -0.136 1.000
0.054 0.076 -0.075 -0.092 0.542 1.000
0.037 0.029 -0.018 -0.054 0.446 0.274 1.00
0.075 0.102  0.089  0.069 0.225 0.267 0.73 1'

cov_drink<-getCov(mat, names=c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"))
model<-'
factor1=~c1+c2
factor2=~c3+c4
factor3=~c5+c6
factor4=~c7+c8
'

cfa_drink<-sem(model, sample.cov = cov_drink, sample.nobs = 60)
summary(cfa_drink, fit.measures=T, standardized=T)

# ex6.4
att<-read.csv("attitude.csv", header=T)
att
mat<-'
1.000
0.610 1.000
0.477 0.542 1.000
0.498 0.195 0.108 1.000
0.194 0.534 0.118 0.473 1.000
0.164 0.135 0.446 0.490 0.342 1
'
cov_att<-getCov(mat, names=c("c1", "c2","c3","c4","c5","c6"))
cov_att

model<-'
factor1=~c1+c2+c3
factor2=~c4+c5+c6
c1~~c4
c2~~c5
c3~~c6'

cfa_att<-cfa(model, sample.cov = cov_att, sample.nobs = 100)
summary(cfa_att, fit.measures=T, standardized=T)

att
for (i in 1:dim(att)[1]){
  for (j in 1:dim(att)[2]){
    att[i,j]<-att[j,i]
  }
}
att
pca_att<-prcomp(att, scale. = T)
plot(pca_att$sdev^2, type="l")
points(pca_att$sdev^2)
pca_att$sdev^2

loadings<-pca_att$rotation%*%diag(pca_att$sdev)
loadings

# ex6.5
store<-read.csv("store.csv", header=T)
store
mat<-'
1.000
0.638 1.000
0.430 0.411 1.000
0.547 0.407 0.426 1.000
0.776 0.600 0.355 0.516 1.000
0.561 0.713 0.432 0.363 0.635 1.000
0.368 0.344 0.813 0.393 0.364 0.451 1.000
0.490 0.370 0.418 0.764 0.531 0.366 0.361 1.000
0.676 0.539 0.394 0.509 0.739 0.527 0.372 0.469 1.000
0.522 0.720 0.384 0.349 0.559 0.698 0.365 0.376 0.589 1.000
0.339 0.323 0.791 0.351 0.335 0.366 0.783 0.340 0.365 0.367 1.000
0.376 0.283 0.343 0.667 0.424 0.290 0.360 0.662 0.515 0.333 0.394 1
'

cov_store<-getCov(mat, names=c("A1", "Pro1", "Pri1", "S1","A2", "Pro2", "Pri2", "S2","A3", "Pro3", "Pri3", "S3"))
cov_store
model<-'
factor1=~A1+A2+A3
factor2=~Pro1+Pro2+Pro3
factor3=~Pri1+Pri2+Pri3
factor4=~S1+S2+S3
A1~~Pri1
A1~~Pro1
A1~~S1
Pro1~~Pri1
Pro1~~S1
Pri1~~S1
A2~~Pri2
A2~~Pro2
A2~~S2
Pro2~~Pri2
Pro2~~S2
Pri2~~S2
A3~~Pri3
A3~~Pro3
A3~~S3
Pro3~~Pri3
Pro3~~S3
Pri3~~S3
'

cfa_store<-cfa(model, sample.cov = cov_store, sample.nobs = 250)
summary(cfa_store, fit.measures=T, standardized=T)

# ex6.6
gover<-read.csv("gover.csv", header=T)
gover

for (i in 1:dim(gover)[1]){
  for (j in 1:dim(gover)[2]){
    gover[i,j]<-gover[j,i]
  }
}

pca_gover<-prcomp(gover, scale. = T)
plot(pca_gover$sdev^2, type="l")
points(pca_gover$sdev^2)
pca_gover$sdev^2
gover<-as.matrix(gover)
efa_gover<-factanal(covmat=gover, factors = 2, rotation = "varimax")
efa_gover

# Oblique rotation in r
install.packages("psych")
install.packages("GPArotation")
library(psych)
library(GPArotation)
solution <- fa(r = gover, nfactors = 2, rotate = "oblimin", fm = "pa")
solution
?fa
mat<-'1.0000
0.6008 1.0000
0.4984 0.4749 1.0000
0.1920 0.2196 0.2079 1.0000
0.1959 0.1912 0.2010 0.4334 1.0000
0.3466 0.2979 0.2445 0.3197 0.4207 1'
cov_gover<-getCov(mat, names=c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6"))
model<-'
factor1=~Col1+Col2+Col3
factor2=~Col4+Col5+Col6'

cfa_gover<-cfa(model, sample.cov = cov_gover, sample.nobs = 100)
summary(cfa_gover, fit.measures=T, standardized=T)

# ex6.7
ostrom<-read.csv("ostrom.csv", header=T)
ostrom

mat<-'
1.00
0.57 1.00
0.63 0.62 1.00
0.71 0.59 0.68 1.00
0.67 0.67 0.71 0.79 1.00
0.69 0.62 0.72 0.79 0.81 1.00
0.54 0.39 0.49 0.58 0.51 0.56 1.00
0.59 0.61 0.58 0.60 0.69 0.60 0.43 1.00
0.63 0.50 0.63 0.69 0.67 0.71 0.49 0.56 1'
cov_ost<-getCov(mat, names=c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9"))

model<-'
factor1=~Col1+Col4+Col7
factor2=~Col2+Col5+Col8
factor3=~Col3+Col6+Col9
Col1~~Col2
Col1~~Col3
Col2~~Col3
Col4~~Col5
Col4~~Col6
Col5~~Col6
Col7~~Col8
Col7~~Col9
Col8~~Col9'

cfa_ost<-cfa(model, sample.cov = cov_ost, sample.nobs = 100)
summary(cfa_ost, fit.measure=T, standardized=T)

# ex6.8
cognition<-read.csv("cognition.csv", header=T)
cognition

# ex6.9
test<-read.csv("tests.csv", header=T)
test

mat<-'1.000
0.318 1.000
0.436 0.419 1.000
0.335 0.234 0.323 1.000
0.304 0.157 0.283 0.722 1.000
0.326 0.195 0.350 0.714 0.685 1.000
0.116 0.057 0.056 0.203 0.246 0.170 1.000
0.314 0.145 0.229 0.095 0.181 0.113 0.585 1.000
0.489 0.239 0.361 0.309 0.345 0.280 0.408 0.512 1'

cov_test<-getCov(mat, names=c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "Col7", "Col8", "Col9"))
cov_test

model<-'
factor1=~Col1+Col2+Col3
factor2=~Col4+Col5+Col6
factor3=~Col7+Col8+Col9'

model1_1<-'
factor1=~Col1+Col2+Col3+Col4+Col5+Col6
factor2=~Col7+Col8+Col9
'

cfa_test<-cfa(model, sample.cov = cov_test, sample.nobs = 145)
summary(cfa_test, fit.measures=T, standardized=T)

# CH 7. MDS
# Classical MDS

# City data
city<-read.csv("city.csv", header=T)
city
city<-city[,-1]
city

# cmdscale : function for classical MDS, k indicates that # of dimensions
cmds_city<-cmdscale(city, k=4, eig = T)
cmds_city
plot(cmds_city$points[,1:2])

# For non matrix data, use dist() to get distance matrix

# non - metric MDS
car<-read.csv("car_dissim.csv", header=T)
car
car<-car[,-1]
car
for (i in 1:dim(car)[1]){
  for (j in 1:dim(car)[2]){
    car[i,j]<-car[j,i]
  }
}
library(MASS)
car<-as.matrix(car)
nmds_car<-isoMDS(car, y = cmdscale(car, k=2),k=2, maxit=100)
# Starting points as metric MDS, denoted as component, "y".
nmds_car
plot(nmds_car$points[,1:2])
points<-nmds_car$points
points
rownames(points)<-car[,1]
points
plot(points)
# in isoMDS, STRESS is expressed as %.

car_att<-read.csv("car_att.csv", header=T)
car_att
car_att<-car_att[,-1]
car_att
cor<-cor(car_att, points)
plot(cor)
abline(h=0, v=0)

# Psy data
psy<-read.csv("psych.csv", header=T)
psy
psy<-psy[,-1]
psy
for (i in 1:dim(psy)[1]){
  for (j in 1:dim(psy)[2]){
    psy[i,j]<-psy[i,j]+psy[j,i]
  }
}
psy
for (i in 1:dim(psy)[1]){
  for (j in 1:dim(psy)[2]){
    psy[j,i]<-psy[i,j]
  }
}
psy
for (i in 1:dim(psy)[1]){
  for (j in 1:dim(psy)[2]){
    if (i==j){
      psy[i,j]<-NA
    }else{
      psy[i,j]<-psy[i,j]
    }
  }
}
psy
psy<-as.matrix(psy)
nmds_psy<-isoMDS(psy, y=cmdscale(psy, k=2), k=2, maxit=1000)
nmds_psy
plot(nmds_psy$points)

psy

# INDSCAL model
bf<-read.csv("breakfast.csv", header=T)
bf
in1<-which(bf[,1]==42)
in2<-which(bf[,1]==71)
in3<-which(bf[,1]==101)
in4<-which(bf[,1]==112)

new_bf<-list(bf[in1,-1], bf[in2,-1], bf[in3,-1], bf[in4,-1])

install.packages("smacof")
library(smacof)
install.packages("mice")
?smacofIndDiff
indscal_bf<-smacofIndDiff(new_bf, ndim=2, itmax=1000)
# Group stimulus space
b<-indscal_bf$conf

# Subject space for each subject
plot(a[1],a[4], xlim=c(0,2), ylim=c(0,2))
points(a[5],a[8])
points(a[9],a[12])
points(a[13],a[16])

# Group stimulus space
plot(indscal_bf$gspace)

indscal_bf$confdist
# Group stimulus space
plot(indscal_bf, plot.type = "confplot")
indscal_bf$gspace
# Perceptual map of each indivisual
plot(indscal_bf$gspace%*%indscal_bf$cweights[[1]], xlim=c(-1,1), ylim=c(-1,1))
points(indscal_bf$gspace%*%indscal_bf$cweights[[2]], col="red")
points(indscal_bf$gspace%*%indscal_bf$cweights[[3]], col="blue")
points(indscal_bf$gspace%*%indscal_bf$cweights[[4]], col="green")

# MDPREF model
# prior to MDPREF, building non-metric MDS
movie<-read.csv("movie_critic.csv", header=T)
movie
for (i in 1:dim(movie)[1]){
  for (j in 1:dim(movie)[2]){
    if (movie[i,j]=="."){
      movie[i,j]<-NA
    }
  }
}
movie<-movie[,-1]
movie

# Unfolding analysis
library(smacof)
str(movie)
for (i in 1:6){
  movie[,i]<-as.numeric(movie[,i])
}
movie
?unfolding
unf_movie<-unfolding(movie, ties="secondary")
summary(unf_movie)
plot(unf_movie$conf.row)
abline(v=0, h=0, col="red")  

# MDPREF
pref_movie<-prefscal(movie, ndim=2, ties="primary")
plot(pref_movie)
abline(v=0, h=0, col="green")
summary(pref_movie)
points(pref_movie$conf.row[,1:2], xlim=c(-.7, .7), ylim=c(-1,1))
plot(pref_movie$conf.col[,1:2], col="red")

# ex7.2
sim<-read.csv("similarity.csv", header=T)
library(MASS)
sim<-sim[,-1]
sim$Col14<-rep(NA, 13)
sim
for (i in 1:dim(sim)[1]){
  for (j in 1:dim(sim)[2]){
    if (i==j){
      sim[i,j]<-0
    }
  }
}

sim
for (i in 1:dim(sim)[1]){
  for (j in 1:dim(sim)[2]){
    sim[i,j]<-sim[j,i]
  }
}
sim
sim<-as.matrix(sim)
nmds_sim<-isoMDS(sim, y = cmdscale(sim, 6), k=6, maxit=100)
nmds_sim

# ex7.3
nation<-read.csv("nations.csv", header=T)
nation
rown<-nation[,1]
nation
nation<-nation[, -1]
nation$Col13<-rep(NA, 12)
nation

rown

for (i in 1:dim(nation)[1]){
  for (j in 1:dim(nation)[2]){
    nation[i,j]<-nation[j,i]
  }
}
nation
nation<-as.matrix(nation)

nmds_nation<-isoMDS(nation, y=cmdscale(nation, 2), k=2, maxit=1000)
nmds_nation
points<-nmds_nation$points
rown
rownames(points)<-rown
points
plot(points)
abline(h=0, v=0, col="red")
identify(points)

# ex 7.4
con<-read.csv("consumption.csv", header=T)
con

# Ch.8 Cluster analysis

# MBA_Beer data
beer<-read.csv("mba_beer.csv", header=T)
beer

wss.plot<-function(df,n){
  wss<-numeric(n)
  for (i in 1:n){
    wss[i]<-mean(kmeans(df, i)$withinss)
  }
  plot(wss)
}

wss.plot(beer[,-1], 10)
?kmeans
kmean_beer<-kmeans(beer[,-1], 2, nstart=3)
kmean_beer

# Bimodal/Unmodal
bi<-read.csv("bimodal.csv", header=T)
un<-read.csv("unimodal.csv", header=T)
?hclust
hclust_bi<-hclust(dist(bi), method = "single")
plot(hclust_bi)

hclust_un<-hclust(dist(un), method= "single")
plot(hclust_un)

ward_bi<-hclust(dist(bi), method="ward.D")
plot(ward_bi)

ward_un<-hclust(dist(un), method="ward.D")
plot(ward_un)

wss.plot(bi,10)
kmean_bi<-kmeans(bi, 2)
kmean_bi$cluster

wss.plot(un, 10)
kmean_un<-kmeans(un, 3)
length(kmean_un$withinss)
kmean_un$withinss

# MBA_car data
car1<-read.csv("mba_car_pref1.csv", header=T)
car2<-read.csv("mba_car_pref2.csv", header=T)
car1
dim(car1)
car1<-na.omit(car1)
car1
car1<-car1[,-1]
car1
for (i in 1:dim(car1)[1]){
  for (j in 1:dim(car1)[2]){
    if (car1[i,j]=="."){
      car1[i,j]<-NA
    }
  }
}
car1
car1<-na.omit(car1)
dim(car1)

hclust_car<-hclust(dist(car1), method="ward.D")
plot(hclust_car)
wss.plot(car1, 10)

kmean_car<-kmeans(car1, 5)
kmean_car$cluster

car2

for (i in 1:dim(car2)[1]){
  for (j in 1:dim(car2)[2]){
    if (car2[i,j]=="."){
      car2[i,j]<-NA
    }
  }
}
car2
sum(is.na(car2))
car2<-na.omit(car2)
dim(car2)

kmean_car_val<-kmeans(car2, 5)
kmean_car_val
sum(abs(a-b))

# ex8.2
# Understanding natural modality using KNN model
iris
new_iris<-iris
new_iris
library(class)
index<-sample(dim(new_iris)[1], 120, replace = F)
?knn
model<-knn(new_iris[index,1:4], new_iris[-index,1:4],cl=iris[index,5], k=3)
model

# Using hclust
hclust_iris<-hclust(dist(iris[,1:4]), method="ward.D")
plot(hclust_iris)

wss.plot(iris[,1:4], 10)

# kmeans
kmean_iris<-kmeans(iris[,1:4], 3)
kmean_iris

# ex8.3
sim<-read.csv("similarity.csv", header=T)
sim
for (i in 1:dim(sim)[1]){
  for (j in 1:dim(sim)[2]){
    if (sim[i,j]==79){
      sim[i,j]<-NA
    }
  }
}
sim
dist(sim)
hclust_sim<-hclust(dist(sim), method="ward.D")
plot(hclust_sim)
kmeans(dist(sim), 4, method="euclidean")

# ex8.4
store<-read.csv("store_share.csv", header=T)
store
hclust_store<-hclust(dist(store), method="ward.D2")
plot(hclust_store)
wss.plot(store, 10)

kmean_store<-kmeans(store, 4)
dist(store)

# ex8.5
shop<-read.csv("shopping.csv", header=T)
shop
di
hclust_shop<-hclust(as.dist(1/shop[,-1]), method="ward.D")

shop
wss.plot(dist, 10)
kmean_shop<-kmeans(dist, 3)
kmean_shop
dist<-dist(shop[,-1], method="manhattan")
?dist
dist<-as.matrix(dist)
dist
for (i in 1:dim(dist)[1]){
  for (j in 1:dim(dist)[2]){
    dist[i,j]<-dist[j,i]
  }
}

library(cluster)
library(fpc)
# Visualize clusters
clusplot(dist, kmean_shop$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# ex8.6
sw<-read.csv("store_switch.csv", header=T)
dist<-as.dist(1/sw[,-1])
hclust_sw<-hclust(dist, method="ward.D")
plot(hclust_sw)

# ex8.7
basket<-read.csv("basket.csv", header=T)
basket
new_ba<-scale(basket[,-1])
hclust_ba<-hclust(dist(new_ba), method="ward.D")
plot(hclust_ba)

# ex8.8
book<-read.table("factbook.txt", header=T)
new_book<-scale(book)

hclust_book<-hclust(dist(new_book[,1:5]), method="ward.D")
plot(hclust_book)                    
wss.plot(new_book[,1:5],10)
kmean_book<-kmeans(new_book[,1:5], 4)
kmean_book
clusplot(new_book[,1:5], kmean_book$cluster, color=TRUE, shade=TRUE, lines=0)

# ex8.9
food<-read.csv("intl_foods.csv", header=T)
food
colnames(food)<-c("con","GC", "IC", "TB", "SS", "BP", "SP", "ST" ,"IP", "FF" ,"VF"
                  ,"AF", "OF", "FT", "JS" ,"CG", "BR", "ME" ,"OO", "YT", "CD")
str(food)
dist(food)
hclust_food<-hclust(dist(food), method="ward.D")
plot(hclust_food)

# ex8.10
coffee<-read.csv("coffee.csv", header=T)
coffee
as.dist(coffee[,-1])

hclust_coffee<-hclust(dist(coffee[,-1]), method="ward.D")
plot(hclust_coffee)

kmean_coffee<-kmeans(dist(coffee[,-1]), 3)
kmean_coffee
clusplot(coffee[,-1], kmean_coffee$cluster, color=TRUE, shade=TRUE, lines=0)

# ex8.11
dissim<-read.csv("org_dissim.csv", header=T)
dissim

hclust_sim<-hclust(dist(dissim), method="ward.D")
plot(hclust_sim)

kmean_sim<-kmeans(dist(dissim), 3)
kmean_sim

# ex8.12
sam1<-read.csv("square_1.csv", header=T)
sam2<-read.csv("square_2.csv", header=T)
kmean_sam1<-kmeans(sam1, 3)
kmean_sam1
kmean_sam2<-kmeans(sam2, 3)
kmean_sam2
table(kmean_sam1$cluster)
table(kmean_sam2$cluster)

# ex8.13
sam3<-read.csv("round_1.csv", header=T)
sam4<-read.csv("round_2.csv", header=T)
kmean_sam3<-kmeans(sam3, 4)
kmean_sam3
kmean_sam4<-kmeans(sam4, 4)
kmean_sam4

# canonical correlation
fact<-read.table("FACTBOOK.txt", header=T)
fact
cor(fact)
fact
dim(fact)
X<-fact[,1:5]
Y<-fact[,6:10]

cc_fact<-cancor(X, Y)
U<-as.matrix(X)%*%cc_fact$xcoef
# Canonical loadings
cor(X, U)
t<-as.matrix(Y)%*%cc_fact$ycoef
cor(Y, t)
U
plot(t[,1]~U[,1])
plot(t[,2]~U[,2])
plot(t[,3]~U[,3])

# the relationship between X and Y are significant???
# 1. Bartlett's chisquare test
bartest<-function(X,Y){
  cc<-cancor(Y,X)
  lams<-cc$cor^2
  A<-prod(1-lams)
  n<-dim(X)[1]
  p<-dim(X)[2]
  q<-dim(Y)[2]
  V<--((n-1)-(p+q+1)/2)*log(A)
  pval<-1-pchisq(V, df=p*q)
  list(stat=V, p_value=pval)
}

bartest(X,Y)

# 2. Rao's F - test
F.rao<-function(X,Y){
  cc<-cancor(Y, X)
  lams<-cc$cor^2
  A<-prod(1-lams)
  n<-dim(X)[1]; p<-dim(X)[2]; q<-dim(Y)[2]
  t<-(n-1)-(p+q+1)/2; s<-1
  if (p^2+q^2<=5){
    s<-1
  }else{
    s<-sqrt(((p*q)^2-4)/(p^2+q^2-5))
  }
  Ra<-(1-A^(1/s))/A^(1/s)*(1+s*t-p*q/2)/(p*q)
  df1<-p*q
  df2<-1+t*s-p*q/2
  pval<-1-pf(Ra, df1, df2)
  list(stat=Ra, p_value=pval)
}

F.rao(X,Y)

# ex9.3
xmat<-matrix(c(1, 0.33, 0.36,0.33,1,0.19, 0.36,.19,1), nrow=3, byrow=T)
xmat
ymat<-matrix(c(1, 0.34, 0.34, 1), nrow=2, byrow=T)
xymat<-matrix(c(0.17, 0.29, 0.21, 0.41,0.54, 0.27), nrow=3, byrow=T)

mat<-solve(xmat)%*%xymat%*%solve(ymat)%*%t(xymat)
sqrt(eigen(mat)$values)

# ex9.4
cc<-read.csv("cc_example.csv", header=T)
cc
X<-cc[,1:2]
Y<-cc[,3:4]
cor(X, Y)
cc_cc<-cancor(Y, X)
cc_cc
U<-as.matrix(X)%*%matrix(c(0.707,-0.707), nrow=2)
U
T<-as.matrix(Y)%*%matrix(c(0.707,-0.707), nrow=2)
plot(T~U)
cor(T, U)

# ex9.5
fact
X<-fact[,1:5]
Y<-fact[,6:10]
cc_fact<-cancor(Y, X)
cc_fact
U<-as.matrix(X)%*%cc_fact$xcoef
# Canonical loadings
cor(X, U)
t<-as.matrix(Y)%*%cc_fact$ycoef
cor(Y, t)

which(rownames(fact)=="CANNED.HA")
X_new<-X[-259,]
Y_new<-Y[-259,]      

cc_fact2<-cancor(Y_new, X_new)
cc_fact2
new_u<-as.matrix(X_new)%*%cc_fact2$xcoef
new_t<-as.matrix(Y_new)%*%cc_fact2$ycoef
cc_fact
cc_fact2
cor(new_u, new_t)
cor(U, t)

# ex9.6
movie<-read.csv("movie_attrib.csv", header=T)
movie
dc<-movie[,2:4]
attrib<-movie[,5:9]
cc_movie<-cancor(dc, attrib)

# ex9.7
?cancor
service<-read.csv("service_quality.csv", header=T)
name.r<-service[,1]
service<-service[,-1]
service
for (i in 1:dim(service)[1]){
  for (j in 1:dim(service)[2]){
    service[i,j]<-service[j,i]
  }
}
service
rownames(service)<-name.r
service
Ymat<-as.matrix(service[1:4,1:4])
Ymat
Xmat<-as.matrix(service[5:10, 5:10])
Xmat
XYmat<-as.matrix(service[5:10, 1:4])
t(XYmat)
new_mat<-solve(Xmat)%*%XYmat%*%solve(Ymat)%*%t(XYmat)
eigen(new_mat)
r<-eigen(new_mat)$values[1:4]
b_coor<-eigen(new_mat)$vectors[,1:4]
b_coor
a_coor<-1/sqrt(r)*solve(Ymat)%*%t(XYmat)%*%b_coor
a_coor
g<-Ymat%*%a_coor
g
f<-Xmat%*%b_coor
f
# only 2 canonical variates are significant
new_a_coor<-a_coor[1:2]
new_b_coor<-b_coor[1:2]
new_f<-f[,1:2]
new_g<-g[,1:2]

# ex9.8
ch<-read.csv("channels.csv", header=T)
ch
Ymat<-as.matrix(ch[1:6,1:6])
Xmat<-as.matrix(ch[7:12,7:12])
XYmat<-as.matrix(ch[7:12, 1:6])
XYmat
Ymat
Xmat

new_mat<-solve(Xmat)%*%XYmat%*%solve(Ymat)%*%t(XYmat)
new_mat
lams<-eigen(new_mat)$values
b_coor<-eigen(new_mat)$vectors
lams
V<--(199-(6+6+1)/2)*log(prod(1-lams))
1-pchisq(V, 6*6)
V<--(199-(5+5+1)/2)*log(prod(1-lams[-1]))
1-pchisq(V, 5*5)
V<--(199-(4+4+1)/2)*log(prod(1-lams[-c(1,2)]))
1-pchisq(V, 4*4)
lams
b_coor
mat<-solve(Ymat)%*%t(XYmat)%*%solve(Xmat)%*%XYmat
a_coor<-eigen(mat)$vectors
a_coor
f<-Xmat%*%b_coor
g<-Ymat%*%a_coor
lams
f;g
# ex9.9
work<-read.csv("work_satisfaction.csv", header=T)
work
name.r<-work[,1]
work<-work[,-1]
for (i in 1:dim(work)[1]){
  for (j in 1:dim(work)[2]){
    work[i,j]<-work[j,i]
  }
}
work
rownames(work)<-name.r
work
Xmat<-as.matrix(work[1:5, 1:5])
Ymat<-as.matrix(work[6:12, 6:12])
YXmat<-as.matrix(work[6:12, 1:5])
YXmat
mat<-solve(Xmat)%*%t(YXmat)%*%solve(Ymat)%*%YXmat
mat
lams<-eigen(mat)$values
b_coor<-eigen(mat)$vectors
b_coor<--b_coor
b_coor

new_mat<-solve(Ymat)%*%YXmat%*%solve(Xmat)%*%t(YXmat)
a_coor<-eigen(new_mat)$vectors
a_coor
f<-Xmat%*%b_coor
g<-Ymat%*%a_coor
f;g

V<--(783-(5+7+1)/2)*log(prod(1-lams))
1-pchisq(V, 35)
V<--(783-(4+6+1)/2)*log(prod(1-lams[-1]))
1-pchisq(V, 24)
V<--(783-(3+5+1)/2)*log(prod(1-lams[-c(1,2)]))
1-pchisq(V, 15)
# only 2 canonical variates are significant

# ex9.10
cr<-read.csv("creativity.csv", header=T)
cr
name.r<-cr[,1]
cr<-cr[,-1]
for (i in 1:dim(cr)[1]){
  for (j in 1:dim(cr)[2]){
    cr[i,j]<-cr[j,i]
  }
}
cr
rownames(cr)<-name.r
cr
Xmat<-as.matrix(cr[1:6, 1:6])
Ymat<-as.matrix(cr[7:12, 7:12])
YXmat<-as.matrix(cr[7:12, 1:6])

mat<-solve(Xmat)%*%t(YXmat)%*%solve(Ymat)%*%YXmat
eigen(mat)
lams<-eigen(mat)$values
b_coor<--eigen(mat)$vectors
mat_norm<-function(mat){
  for (i in 1:dim(mat)[1]){
    mat[i,]<-mat[i,]/sqrt(sum(mat[i,]^2))
  }
  return(mat)
}

b_coor<-mat_norm(b_coor)
b_coor
Xmat%*%b_coor
new_mat<-solve(Ymat)%*%YXmat%*%solve(Xmat)%*%t(YXmat)
a_coor<-eigen(new_mat)$vectors
a_coor
a_coor<-mat_norm(a_coor)
a_coor
g<-Ymat%*%a_coor
f;g

# ex9.11
prob<-read.csv("problem_solving.csv", header=T)
prob
name.r<-prob[,1]
prob<-prob[,-1]
prob
for (i in 1:dim(prob)[1]){
  for (j in 1:dim(prob)[2]){
    prob[i,j]<-prob[j,i]
  }
}
prob
rownames(prob)<-name.r
colnames(prob)<-name.r
prob
Xmat<-as.matrix(prob[1:4, 1:4])
Ymat<-as.matrix(prob[5:11, 5:11])
YXmat<-as.matrix(prob[5:11, 1:4])
YXmat
mat<-solve(Xmat)%*%t(YXmat)%*%solve(Ymat)%*%YXmat
lams<-eigen(mat)$values

b_coor<-eigen(mat)$vectors
b_coor

mat2<-solve(Ymat)%*%YXmat%*%solve(Xmat)%*%t(YXmat)
a_coor<-eigen(mat2)$vectors
a_coor<-a_coor[,1:4]
f<-Xmat%*%b_coor
g<-Ymat%*%a_coor
f;g

V<--(111-(4+7+1)/2)*log(prod(1-lams))
1-pchisq(V, 28)
V<--(111-(3+6+1)/2)*log(prod(1-lams[-1]))
1-pchisq(V, 18)

# ex9.12
modern<-read.csv("modernization.csv", header=T)
modern
name.r<-modern[,1]
modern<-modern[,-1]
dim(modern)
for (i in 1:dim(modern)[1]){
  for (j in 1:dim(modern)[2]){
    modern[i,j]<-modern[j,i]
  }
}
modern
rownames(modern)<-name.r
colnames(modern)<-name.r
modern
Xmat<-as.matrix(modern[1:2,1:2])
Ymat<-as.matrix(modern[3:5,3:5])
YXmat<-as.matrix(modern[3:5,1:2])

mat<-solve(Xmat)%*%t(YXmat)%*%solve(Ymat)%*%YXmat
lams<-eigen(mat)$values
b_coor<-eigen(mat)$vectors
sqrt(lams)
f<-Xmat%*%b_coor
f

# ex9.13
ad<-read.csv("advisor_explore.csv", header=T)
ad
X<-ad[,1:9]
Y<-ad[,10:18]
X; Y
cc_ad<-cancor(Y, X)
cc_ad
plot(cc_ad$cor^2)
cc_ad$cor[1:3]^2
X
pca_X<-prcomp(scale(X))
min_X<-pca_X$x[,1:3]

pca_Y<-prcomp(scale(Y))
pca_Y
min_Y<-pca_Y$x[,1:3]

cc_ad2<-cancor(min_Y, min_X)
cc_ad2

# ex9.14
car<-read.csv("car_satisfaction.csv", header=T)
car
name.r<-car[,1]
car<-car[,-1]
for (i in 1:dim(car)[1]){
  for (j in 1:dim(car)[2]){
    car[i,j]<-car[j,i]
  }
}
car
rownames(car)<-name.r
colnames(car)<-name.r
car
Xmat<-as.matrix(car[1:11,1:11])
Ymat<-as.matrix(car[12:14,12:14])
YXmat<-as.matrix(car[12:14,1:11])

mat<-solve(Xmat)%*%t(YXmat)%*%solve(Ymat)%*%YXmat
b_coor<-eigen(mat)$vectors
lams<-eigen(mat)$values

# only 2 canonical variates are significant
new_mat<-solve(Ymat)%*%YXmat%*%solve(Xmat)%*%t(YXmat)
a_coor<-eigen(new_mat)$vectors

-Xmat%*%b_coor[,1:3]
-Ymat%*%a_coor[,1:3]
lams

# Ch  10. SEM
# Structural Equation model for coupon usage data
coupon<-read.csv("coupon_usage_a.csv", header=T)
coupon
library(lavaan)
mat<-'4.389
3.792 4.410
1.935 1.855 2.385
1.454 1.453 0.989 1.914
1.087 1.309 0.841 0.961 1.48
1.623 1.701 1.175 1.279 1.22 1.971'

cov_coupon<-getCov(mat, names=c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6"))
cov_coupon

model<-'
Y1=~Col1+Col2
Y2=~Col3
X=~Col4+Col5+Col6
Y1~a*X
Y2~b*X+c*Y1
'

model_coupon<-sem(model, sample.cov = cov_coupon, sample.nobs = 85)
summary(model_coupon, fit.measures=T, standardized=T)
?sem

job<-read.csv("job_satisfaction.csv", header=T)
job[c(2,3,6,7), c(3,4,7,8)]
# n=106
mat<-'
1.000
0.647 1.000
0.297 0.288 1.000
0.254 0.284 0.548 1
'

cov_job<-getCov(mat, names=c("sat1","sat2","est1", "est2"))
cov_job

model<-'
Y=~sat1+sat2
X=~est1+est2
Y~a*X'

sem_job<-sem(model, sample.cov = cov_job, sample.nobs = 106)
summary(sem_job, fit.measures=T, standardized=T)
sem_job

# MIMIC model which is equivalent to canonical correlation analysis
model<-'
factor1=~est1
factor2=~est2
gam=~sat1+sat2
gam~a*factor1+b*factor2
'
sem_job2<-sem(model, sample.cov = cov_job, sample.nobs = 106)
summary(sem_job2, fit.measures=T, standardized=T)

adop<-read.csv("adoption.csv", header=T)
adop
dim(adop)
for (i in 1:dim(adop)[1]){
  for (j in 1:dim(adop)[2]){
    if (adop[i,j]=="."){
      adop[i,j]<-NA
    }else{
      adop[i,j]<-adop[i,j]
    }
  }
}
adop
adop<-na.omit(adop)
dim(adop)
adop

mat<-'
1
0.599 1
0.478 0.571 1
0.464 0.580 0.763 1
0.360 0.481 0.628 0.720 1
0.263 0.110 0.187 0.208 0.122 1
0.248 0.156 0.124 0.139 0.051 0.437 1
0.222 0.104 0.155 0.191 0.054 0.542 0.421 1
'

cov_adop<-getCov(mat, names=c("Y1", "Y2", "X1", "X2", "X3", "X4","X5","X6"))
model<-'
factor1=~X1+X2+X3
factor2=~X4+X5+X6
gam=~Y1+Y2
gam~a*factor1+b*factor2
'

sem_adop<-sem(model, sample.cov = cov_adop, sample.nobs = 188)
summary(sem_adop, fit.measures=T, standardized=T)

# ex10.2, n = 151
service[-c(3,4), -c(3,4)]
mat<-'
1.00
0.63 1.00
0.38 0.33 1.00
0.42 0.28 0.57 1.00
0.37 0.30 0.48 0.59 1.00
0.30 0.36 0.15 0.29 0.30 1.00
0.45 0.37 0.29 0.41 0.35 0.44  1.00
0.56 0.56 0.18 0.33 0.30 0.46  0.63  1.00'

cov_ser<-getCov(mat, names=c("Y1","Y2","X1","X2","X3","X4","X5","X6"))
str(cov_ser)

model<-'
factor1=~X1+X2+X3
factor2=~X4+X5+X6
gam=~Y1+Y2
gam~a*factor1+b*factor2'
sem_ser<-sem(model, sample.cov = cov_ser, sample.nobs = 151)
summary(sem_ser, fit.measures=T, standardized=T)

# ex10.3, nobs = 200
ch
colnames(ch)<-c("Y1", "Y2","Y3","Y4","Y5","Y6","X1","X2","X3","X4","X5","X6")
ch<-as.matrix(ch)
ch

mat<-'
1.00
0.47  1.00
0.47  0.83  1.00
0.06 -0.02  0.02  1.00
0.00  0.02 -0.02  0.60  1.00
0.03  0.00 -0.03  0.65  0.72  1.00
-0.09 -0.19 -0.23 -0.47 -0.38 -0.41  1.00
-0.06 -0.12 -0.16 -0.38 -0.34 -0.30  0.81  1.00
-0.02 -0.05 -0.15 -0.41 -0.39 -0.33  0.73  0.77  1.00
-0.02 -0.21 -0.22 -0.41 -0.31 -0.28  0.49  0.43  0.47  1.00
-0.07 -0.14 -0.15 -0.42 -0.28 -0.27  0.41  0.39  0.47  0.52  1.00
-0.02 -0.15 -0.16 -0.49 -0.43 -0.42  0.48  0.46  0.55  0.60  0.73  1.00'
cov_ch<-getCov(mat, names=c("Y1", "Y2","Y3","Y4","Y5","Y6","X1","X2","X3","X4","X5","X6"))
cov_ch
model<-'
gam1=~Y1+Y2+Y3
gam2=~Y4+Y5+Y6
factor1=~X1+X2+X3
factor2=~X4+X5+X6
gam1=~a*factor1+b*factor2
gam2=~c*factor1+d*factor2
factor1~~factor2'

sem_ch<-sem(model, sample.cov = cov_ch, sample.nobs = 200)
summary(sem_ch, fit.measures=T, standardized=T)

# ex10.4
de_m<-read.csv("debate_men.csv", header=T)
de_w<-read.csv("debate_women.csv", header=T)
de_m
de_w

mat_men<-'
21.1
18.5 21.4
19.3 19.2 21.1
5.3  5.6  5.7  7.9
5.0  4.8  5.2  5.2  6.5
5.1  5.3  5.2  0.2  0.0  6.3
5.4  5.6  5.3  0.1  0.0  4.7  6.7
'

mat_women<-'
39.4
37.1 39.2
37.9 37.6 40.5
11.1 11.2 11.4  7.8
10.0  9.8 10.3  5.2  6.9
5.3  5.7  5.5  0.2  0.9  6.8
5.6  6.1  5.5  0.1  0.0  4.5  6.7'

cov_men<-getCov(mat_men, names=c("S1","S2","S3","P1","P2","Q1","Q2"))
cov_women<-getCov(mat_women, names=c("S1","S2","S3","P1","P2","Q1","Q2"))

model_debate<-'
sol=~S1+S2+S3
P=~P1+P2
Q=~Q1+Q2
sol~a*P+b*Q
'

sem_men<-sem(model_debate, sample.cov = cov_men, sample.nobs = 154)
sem_women<-sem(model_debate, sample.cov = cov_women, sample.nobs = 125)
summary(sem_men, standardized=T, fit.measures=T)
summary(sem_women, standardized=T, fit.measures=T)

# ex10.5
att<-read.csv("att_int_beh.csv", header=T)
att
mat<-'
1.000
0.534 1.000
0.364 0.407 1.000
0.334 0.329 0.660 1.000
0.244 0.260 0.285 0.332 1.000
0.142 0.211 0.292 0.363 0.432 1
'

cov_att<-getCov(mat, names=c("X1","X2","Y1","Y2","Y3","Y4"))

model<-'
factor=~X1+X2
gam1=~Y1+Y2
gam2=~Y3+Y4
gam1~a*factor
gam2~b*factor+c*gam1
'

sem_att<-sem(model, sample.cov = cov_att, sample.nobs = 236)
summary(sem_att, fit.measures=T, standardized=T)

# ex10.6
coupon_a<-read.csv("coupon_usage_a.csv", header=T)
coupon_a
library(lavaan)
mat_a<-'4.389
3.792 4.410
1.935 1.855 2.385
1.454 1.453 0.989 1.914
1.087 1.309 0.841 0.961 1.48
1.623 1.701 1.175 1.279 1.22 1.971'

cov_coupon_a<-getCov(mat, names=c("Y1","Y2","Y3","X1","X2", "X3"))

coupon_b<-read.csv("coupon_usage_b.csv", header=T)

mat_b<-'
3.730
3.208 3.436
1.687 1.675 2.171
0.621 0.616 0.606 1.373
1.063 0.864 0.428 0.671 1.397
0.895 0.818 0.595 0.912 0.663 1.498
'

cov_coupon_b<-getCov(mat, names=c("Y1","Y2","Y3","X1","X2", "X3"))

model<-'
factor1=~X1+X2+X3
gam1=~Y1+Y2
gam2=~Y3
gam1~a*factor1
gam2~b*factor1+c*gam1'

sem_coupon_a<-sem(model, sample.cov = cov_coupon_a, sample.nobs = 85)
sem_coupon_b<-sem(model, sample.cov = cov_coupon_b, sample.nobs = 64)
summary(sem_coupon_a, fit.measures=T, standardized=T)
summary(sem_coupon_b, fit.measures=T, standardized=T)

# ex 10.7
math<-read.csv("math_attitude.csv", header=T)
math
math

model<-'
factor1=~Col2+Col3+Col4
factor2=~Col5+Col6+Col7
gam1=~Col8+Col9+Col10
Col2~~Col5
Col3~~Col6
Col4~~Col7
gam1~a*factor1+b*factor2
Col11~c*factor1+d*factor2+e*gam1
'

sem_math<-sem(model, data=math)
summary(sem_math, fit.measures=T, standardized=T)

# Ch 11. ANOVA, ANCOVA, MANOVA, MANCOVA
food<-read.csv("newfood.csv", header=T)
food
food$sale<-apply(food[,1:3], 1, sum)
food
food$Col4<-factor(food$Col4)
food$Col5<-factor(food$Col5)

# 2 Way Anova
ano_model1<-aov(sale~Col4*Col5, data=food)
summary(ano_model1)

# 2 Way Ancova
plot(food$Col8, food$sale)
# Covariate goes first in specifying model
anco_model1<-aov(sale~Col8+Col4*Col5, data=food)
summary(anco_model1)

# MANOVA
copy<-read.csv("ad_copy.csv", header=T)
copy
colnames(copy)<-c("X","Y1","Y2")
sep_Y1<-aov(Y1~X, data=copy)
sep_Y2<-aov(Y2~X, data=copy)
summary(sep_Y1)
summary(sep_Y2)
plot(Y2~Y1, data=copy)
?manova

man_model<-manova(cbind(Y1,Y2)~X, data=copy)
summary(man_model) # where Pillai denote 1-Wilks's A

# Testing specific hypothesis : Using Finland liquor
liquor<-read.csv("finland.csv", header=T)
liquor
colnames(liquor)<-c("X","Y","co")
liquor$X<-factor(liquor$X)
model<-aov(Y~X, data=liquor)
summary(model)

model2<-aov(Y~co+X, data=liquor)
summary(model2)
model2

liquor
new<-numeric(dim(liquor)[1])
for (i in 1:dim(liquor)[1]){
  if (liquor$X[i]==2){
    new[i]<-1
  }else{
    new[i]<-liquor$X[i]
  }
}
new
liquor$new<-new
con<-aov(Y~co+new,data=liquor)
summary(con)
anova(con,model2)

model2
model3<-aov(Y~X*co, data=liquor)

anova(model2, model3)

# MANOVA for repeated designs
food<-read.csv("newfood.csv", header=T)
food
colnames(food)<-c("Y1","Y2","Y3","P","Ad","col6","col7","size")
food$P<-relevel(factor(food$P), ref = 3)
food$Ad<-factor(food$Ad)

f<-(1524/2)/(1176/21)
1-pf(f, 2, 21)

# ex11.4
pre<-read.csv("preferences.csv", header=T)
pre
tail(pre)

# ex11.5
shelf<-read.csv("shelf_test.csv", header=T)
shelf
colnames(shelf)<-c("H","S","Y")
shelf$H<-factor(shelf$H)
shelf$S<-factor(shelf$S)

ano_model<-aov(Y~H+S, data=shelf)
summary(ano_model)
ano_model2<-aov(Y~H*S, data=shelf)
summary(ano_model2)
anova(lm(Y~H*S, data=shelf))

# ex11.6
game<-read.csv("competitive_game.csv", header=T)
game
colnames(game)<-c("T","Y1","Y2", "E")
game$T<-relevel(factor(game$T), ref=1)

man_game<-manova(cbind(Y1, Y2)~T, data=game)
summary(man_game)

man_game2<-manova(cbind(Y1, Y2)~E+T, data=game)
summary(man_game2)

# ex11.7
claim<-read.csv("ad_claim_test.csv", header=T)
claim
colnames(claim)<-c("obs","T1","T2","Y1","Y2","Y3","Y4")
claim

man_c<-manova(cbind(Y1,Y2,Y3,Y4)~T1*T2, data=claim)
summary(man_c)

X<-claim[,2:3]
Y<-claim[,4:7]
X
Y

cc_c<-cancor(X,Y)
U<-as.matrix(X)%*%cc_c$xcoef
t<-as.matrix(Y)%*%cc_c$ycoef
cor(X, U)
cor(Y, t)
cc_c

# ex11.8
fish<-read.csv("fish.csv", heade=T)
fish
colnames(fish)<-c("Treat","Y1", "Y2","Y3","Y4")
fish

man_fish<-manova(cbind(Y1,Y2,Y3,Y4)~Treat, data=fish)
summary(man_fish)

# ex11.9
eval<-read.csv("expect_eval.csv", header=T)
eval
for (i in 1:dim(eval)[1]){
  for (j in 1:dim(eval)[2]){
    if (eval[i,j]=="."){
      eval[i,j]<-NA
    }
  }
}
eval[,6]

eval<-na.omit(eval)

for (i in 2:dim(eval)[2]){
  eval[,i]<-factor(eval[,i])
}

dim(eval)
eval[,5]<-as.numeric(eval[,5])
eval[,6]<-as.numeric(eval[,6])
eval[,2]
colnames(eval)<-c("id","X1","X2","X3","Y1","Y2")

aov1<-aov(Y1~X2*X3, data=eval)
summary(aov1)
cognition
eval$z<-z
dim(eval)
for (i in 1:dim(cognition)[1]){
  for (j in 1:dim(cognition)[2]){
    if (cognition[i,j]=="."){
      cognition[i,j]<-NA
    }
  }
}
cognition<-na.omit(cognition)
cognition

for (i in 1:dim(cognition)[2]){
  cognition[,i]<-as.numeric(cognition[,i])
}

str(cognition)

pca.cog<-prcomp(cognition[,-1])
pca.cog

wss.plot(cognition[,-1],30)

z<-apply(as.matrix(cognition[,-1])%*%pca.cog$rotation, 1, sum)
z<-as.numeric(z)
z

man_eval<-manova(cbind(Y1,Y2)~X2*X3, data=eval)
summary(man_eval)

# ex11.10
brand<-read.csv("brand_study.csv", header=T)
brand
for (i in 1:dim(brand)[1]){
  for (j in 1:dim(brand)[2]){
    if (brand[i,j]=="."){
      brand[i,j]<-NA
    }
  }
}
brand

brand<-na.omit(brand)
brand[,3]<-as.numeric(brand[,3])
brand[,4]<-as.numeric(brand[,4])

aov3<-aov(Col3~Col7+Col5, data=brand)
summary(aov3)

man_br<-manova(cbind(Col3, Col4)~Col8+Col7+Col5, data=brand)
summary(man_br)

# ex11.11
psy<-read.csv("psych_orientation.csv", header=T)
colnames(psy)<-c("Treat","Anx", "De","Ang")
man_psy<-manova(cbind(Anx, De, Ang)~Treat, data=psy)
summary(man_psy)

# ex11.12
soc<-read.csv("soc_skills.csv", header=T)
soc
colnames(soc)<-c("X","Y1","Y2","Y3","Y4")
soc
soc[,1]<-as.factor(soc[,1])

man_soc<-manova(cbind(Y1, Y2,Y3,Y4)~X, data=soc)
summary(man_soc)

# discriminant analyze
book1<-read.csv("books_1.csv", header=T)
book2<-read.csv("books_2.csv", header=T)
library(MASS)
apply(book1, 2, mean)
apply(book2, 2, mean)
book<-rbind(book1, book2)
str(book1)
book1[,4]<-factor(book1[,4])
levels(book1[,4])
lda1<-lda(Col4~Col2+Col3,data=book1)
lda1

# discriminant scores
score<-as.matrix(book1[,c(2,3)])%*%lda1$scaling
score
lda1

# Test significant difference between the centroids
install.packages("Hotelling")
library(Hotelling)
?hotel.test

k1<-which(book1[,4]==1)
k2<-which(book1[,4]==0)
a<-hotelling.test(book1[k2,c(2,3)],book1[k1, c(2,3)])
a

# Multiple discriminant analysis
# Fisher's discriminant analysis
real<-read.csv("real_estate.csv", header=T)
real[,2]<-scale(real[,2])
real[,3]<-scale(real[,3])
real[,4]<-scale(real[,4])

# ex12.5
lda.real<-lda(Col1~., data=real)
lda.real
str(real)


summary(lfda_real)
print(lfda_real)
?lda
str(real)
lda_real<-lda(Col1~., data=real)
lda_real
plot(lda_real)

# ex12.6
book1
str(book1)


qda_book<-qda(Col4~Col2+Col3, data=book1)
lda_book<-lda(Col4~Col2+Col3, data=book1)
lda_book
qda_book

# ex12.7
dep<-read.csv("depression.csv", header=T)
str(dep)
dep[,7]<-as.factor(dep[,7])
dep<-dep[,-1]
dep
lda_dep<-lda(Col7~., data=dep)
lda_dep
pred<-predict(lda_dep, dimen=1)$class
table(dep$Col7, pred)

# ex12.8
gun<-read.csv("gun_control.csv", header=T)
gun
str(gun)
gun[,1]<-factor(gun[,1])
str(gun)
lda_gun<-lda(Col1~., data=gun)
lda_gun
pred_gun<-predict(lda_gun, dimen=1)
table(pred_gun$class, gun[,1])

# ex12.9
school<-read.csv("b_school.csv", header=T)
school
str(school)
school[,2]<-factor(school[,2])
school<-school[,-c(1,3,4,5,6,10,11,12,13)]
school
str(school)
school
lda_school<-lda(Col2~., data=school)
lda_school
plot(lda_school)
x<-data.frame(3.9, 710, 2, 1, 4)
dim(x)
colnames(x)<-c("Col7","Col8","Col9","Col14","Col15")
predict(lda_school, newdata=x)

# ex12.10
mag<-read.csv("magazines.csv", header=T)
mag
str(mag)
mag[,2]<-factor(mag[,2])
mag<-mag[,-c(1,3,4,5,6)]
mag
lda_mag<-lda(Col2~., data=mag)
lda_mag
plot(lda_mag)

# ex12.11
org_dis<-read.csv("org_dissim.csv", header=T)
org_dis

plot(hclust(dist(org_dis)))
kmean_dis<-kmeans(org_dis, 3)
kmean_dis

org_att<-read.csv("org_attrib.csv", header=T)
org_att$factor<-c(kmean_dis$cluster, NA)
org_att<-org_att[,-1]
org_att

lda_att<-lda(factor~., data=org_att)
lda_att

predict(lda_att, newdata=org_att[31,-11])

# ex12.13
pro<-read.csv("new_products.csv", header=T)
pro[,4]<-factor(pro[,4])
pro<-pro[,-1]
lda_pro<-lda(Col4~., data=pro)
lda_pro
plot(lda_pro)
pre<-predict(lda_pro)$class
table(pre, pro[,3])

# ex12.14
fam<-read.csv("family_cars.csv", header=T)
fam
str(fam)
fam[,4]<-factor(fam[,4])
str(fam)

lda_fam<-lda(Col4~., data=fam)
lda_fam
plot(lda_fam)

table(predict(lda_fam)$class, fam[,4])
dim(fam)

# ex12.15
iris
str(iris)
iris[,1]<-factor(iris[,1])
lda_iris<-lda(Col1~., data=iris)
lda_iris

table(predict(lda_iris)$class, iris$Col1)



