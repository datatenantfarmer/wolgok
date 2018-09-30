cormat<-matrix(c(1,0.562,0.704,0.562,1,0.304,0.704,0.304,1),3,3,byrow=T)
# Using lagrange multiplers & diagonalization & eigen problems
lams<-eigen(cormat)$values
U<--eigen(cormat)$vectors
lams
U
# principal conponent loadings
U%*%diag(sqrt(lams))

gsp<-scale(gsp)
R<-cor(gsp)
pcgsp<-prcomp(gsp)
pcgsp$sdev^2

lams<-eigen(R)$values
U<--eigen(R)$vectors
U%*%diag(sqrt(lams))
lams
gsp2
gsp2<-gsp2[,-1]
gsp2<-scale(gsp2)
pcgsp2<-prcomp(gsp2)
pcloading<-pcgsp2$rotation%*%diag(pcgsp2$sdev)
colnames(gsp2)

plot(pcloading[,2]~pcloading[,1])
abline(h=0, v=0, col="red")
gsp2[,-1]%*%pcgsp2$rotation
plot(pcgsp2$x[,2]~pcgsp2$x[,1])
plot(pcgsp2$sdev^2, type="l")
points(pcgsp2$sdev^2)
pcgsp2$sdev^2
# explaned varince
rowSums(pcloading[,1:3]^2)


rte<-rte[,-c(1,2)]
pc_rte<-prcomp(rte)
plot(pc_rte$sdev^2, type="l")
points(pc_rte$sdev^2)

pc_rte$sdev^2

fa_rte<-factanal(rte, factors=4, rotation = "varimax", scores = "regression")
rowSums(fa_rte$loadings^2)
fa_rte
1-fa_rte$uniquenesses
fa_rte$scores

# uniqueness : var(specific factors)

test
test
lams<-eigen(test)$values
U<-eigen(test)$vetors
plot(lams, type="l")
points(lams)
test<-as.matrix(test)
efa_test<-factanal(covmat=test, factors=2, rotation="varimax")
efa_test

efa_test_ob<-fa(r=test, nfactors=2, rotate="oblimin", fm="pa")
efa_test_ob

library(MASS)
library(base)
x<-matrix(c(6,6,0,4,5,2,10,8,1,12,9,3,8,5,4), nrow=5, byrow=T)
t(x)
x
x<-matrix(c(-2,-.6,-2,-4,-1.6,0,2,1.4,-1,4,2.4,1,0,-1.6,2), nrow=5, byrow=T)
x
cov(x)
t(x)%*%x/4
cor(x)
x1<-c(6,4,10,12,8)
x2<-c(6,5,8,9,5)
x3<-c(0,2,1,3,4)
var(3*x1-2*x2+x3)
var(x1-x3)

A<-matrix(c(-1,3,4,2),2,2,byrow=T)
B<-matrix(c(4,-3,1,2,-2,0),nrow=3, byrow=T)
B%*%A
t(A)%*%t(B)
c<-c(5,-4,2)
t(c)%*%B

A<-matrix(c(9,-2,-2,6), 2,2, byrow=T)
eigen(A)

matrix(c(2,1,-1,2),2,2,byrow=T)%*%diag(c(0.1, 0.2))%*%matrix(c(2,-1,1,2), 2,2,byrow=T)/5
eigen(solve(A))
64+81
A<-matrix(c(4,8,8,3,6,-9), nrow=2,byrow=T)
eigen(t(A)%*%A)
t(A)%*%A
144*125/150
0.1825742*5
95/5
145/5
eigen(A%*%t(A))
svd(A)
sqrt(150)
sqrt(120)
A<-matrix(c(1,-1,0,-1,2,-1,0,-1,1),3,3,byrow=T)
eigen(A)
gamma(11)
