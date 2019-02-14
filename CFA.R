setwd("/Users/chanheelee/Desktop")
wine<-read.table("wine.txt")
wine

library(sem)
colnames(wine)<-c("ex1","ex2","ex3","ex4","ex5")
dim(wine)

model_wine<-specifyModel()
S->ex1, lam1, NA
S->ex2, lam2, NA
S->ex3, lam3, NA
S->ex4, lan4, NA
S->ex5, lam5, NA
ex1<->ex1, theta11, NA
ex2<->ex2, theta22, NA
ex3<->ex3, theta33, NA
ex4<->ex4, theta44, NA
ex5<->ex5, theta55, NA
S<->S, NA, 1

cfa_wine<-sem(model_wine, cor(wine), 59)
summary(cfa_wine)

model_wine2<-specifyModel()
S->ex1, lam1, NA
S->ex2, lam1, NA
S->ex3, lam1, NA
S->ex4, lam1, NA
S->ex5, lam1, NA
ex1<->ex1, theta11, NA
ex2<->ex2, theta11, NA
ex3<->ex3, theta11, NA
ex4<->ex4, theta11, NA
ex5<->ex5, theta11, NA
S<->S, NA, 1

cfa_wine2<-sem(model_wine2, cor(wine), 59)
anova(cfa_wine2, cfa_wine)
summary(cfa_wine2)

install.packages("DiagrammeR")
library(DiagrammeR)
path.diagram(cfa_wine2)

# composite index
cfa_wine2$coef
as.matrix(wine)%*%solve(cor(wine))%*%rep(cfa_wine2$coeff[1],5)

trait=readMoments(diag=TRUE, names=c("AL","PL","PriceL","ServL",
                                     "AD","PD","PriceD","ServD","AS","PS","PriceS","ServS"),
                  text="
                  1.000
                  0.638   1.000
                  0.430   0.411   1.000
                  0.547   0.407   0.426   1.000
                  0.776   0.600   0.355   0.516   1.000
                  0.561   0.713   0.432   0.363   0.635   1.000
                  0.368   0.344   0.813   0.393   0.364   0.451   1.000
                  0.490   0.370   0.418   0.764   0.531   0.366   0.361   1.000
                  0.676   0.539   0.394   0.509   0.739   0.527   0.372   0.469   1.000
                  0.522   0.720   0.384   0.349   0.559   0.698   0.365   0.376   0.589   1.000
                  0.339   0.323   0.791   0.351   0.335   0.366   0.783   0.340   0.365   0.367   1.000
                  0.376   0.283   0.343   0.667   0.424   0.290   0.360   0.662   0.515   0.333   0.394   1.000
                  ")

trait=trait+t(trait)
diag(trait)=1

trait

int<-trait[c(1,2,5,6,9,10),c(1,2,5,6,9,10)]
int

model_store<-specifyModel()
A -> AL, lam1, NA
A -> AD, lam2, NA
A -> AS, lam3, NA
P -> PL, lam4, NA
P -> PD, lam5, NA
P -> PS, lam6, NA
A<->P, phi12, NA
A<->A, NA, 1
P<->P, NA, 1
AL<->AL, theta11, NA
PL<->PL, theta22, NA
AD<->AD, theta33, NA
PD<->PD, theta44, NA
AS<->AS, theta55, NA
PS<->PS, theta66, NA

cfa_store<-sem(model_store, int, 250)
summary(cfa_store)

model_store2<-specifyModel()
A -> AL, lam1, NA
A -> AD, lam2, NA
A -> AS, lam3, NA
P -> PL, lam4, NA
P -> PD, lam5, NA
P -> PS, lam6, NA
A<->P, phi12, NA
A<->A, NA, 1
P<->P, NA, 1
L->AL, lamL, NA
L->PL, lamL, NA
D->AD, lamD, NA
D->PD, lamD, NA
S->AS, lamS, NA
S->PS, lamS, NA
L<->L, NA, 1
D<->D, NA, 1
S<->S, NA, 1
AL<->AL, theta11, NA
PL<->PL, theta22, NA
AD<->AD, theta33, NA
PD<->PD, theta44, NA
AS<->AS, theta55, NA
PS<->PS, theta66, NA

cfa_store2<-sem(model_store2, int, 250)
summary(cfa_store2)
anova(cfa_store, cfa_store2)

model_store3<-specifyModel()
A -> AL, lam1, NA
A -> AD, lam2, NA
A -> AS, lam3, NA
P -> PL, lam4, NA
P -> PD, lam5, NA
P -> PS, lam6, NA
A<->P, phi12, NA
A<->A, NA, 1
P<->P, NA, 1
AL<->PL, errorL, NA
AD<->PD, errorD, NA
AS<->PS, errorS, NA
AL<->AL, theta11, NA
PL<->PL, theta22, NA
AD<->AD, theta33, NA
PD<->PD, theta44, NA
AS<->AS, theta55, NA
PS<->PS, theta66, NA


cfa_store3<-sem(model_store3, int, 250)
summary(cfa_store3)
summary(cfa_store2)
anova(cfa_store, cfa_store3)

400*log(.5,2)
400*log(.5,2)
400+(80*log(.4,2)+120*log(.6,2))*2

(-.04)^(1/3)
f<-function(x){
  x^(1/3)
}
x<-seq(-1, 1, 0.01)
plot(f(x)~x, type="l", ylim=c(-1, 1))

a<-read.table("cds2.txt")
a<-as.vector(a)
a<-a$V1
a
b<-numeric(166)
for (i in 1:166){
  b[167-i]<-a[i]
}
b

write.table(b, "cds.txt", row.names = FALSE)

hd<-read.table("hd.csv", header=T, sep=",")

ratio_hot<-sum(hd$hotdogs)/sum(hd$dwellers)
ratio_hot
Mbar<-sum(hd$dwellers)/20
Smys<-sum(((hd$hotdogs/hd$dwellers-ratio_hot)*hd$dwellers)^2)/19
varhat<-(1-20/124)*Smys/20/(Mbar^2)
sqrt(varhat)


k<-28462
ps<-read.csv("ps.csv", header=T, sep=",")
ss<-read.csv("ss_sum2.csv", header=T, sep=",")
ss$w1<-ss$w1/20
ss$w2<-ss$w2/20


srs_unb<-function(y, w1, w2,k){
  t_unb<-w2*y
  y_unb<-w1*sum(t_unb)/k
  return(y_unb)
}

ss1<-read.csv("ss.csv", header=T, sep=",")
ss1

# y1_srs
a<-srs_unb(ss$m_1, 10, ss$w2, 28462)
a
# y2_srs
b<-srs_unb(ss$m_2, 10, ss$w2, 28462)
b
# y3_srs
c<-srs_unb(ss$m_3, 10, ss$w2, 28462)
c

ss1$m1<-rep(m_1, each=20)
ss1$m2<-rep(m_2, each=20)
ss1$m3<-rep(m_3, each=20)
ss$m_1*ss$w2

srs_com1<-function(N,n,y,w1,w2){
  t_hat<-y*w2
  sts<-sum((t_hat-mean(t_hat))^2)/(n-1)
  (N^2)*(1-n/N)*sts/n
}

a1<-mean(srs_com1(250, 25, ss$m_1, ss$w1, ss$w2))
b1<-mean(srs_com1(250, 25, ss$m_2, ss$w1, ss$w2))
c1<-mean(srs_com1(250, 25, ss$m_3, ss$w1, ss$w2))

ss1
attach(ss1)
a<-(y1-m1)^2
b<-(y2-m2)^2
c<-(y3-m3)^2

a<-matrix(a, nc=20, byrow=T)
b<-matrix(b, nc=20, byrow=T)
c<-matrix(c, nc=20, byrow=T)

sys1<-apply(a, 1, sum)/19
sys2<-apply(b, 1, sum)/19
sys3<-apply(c, 1, sum)/19
M<-ss$w2*20

srs_com2<-function(N, n, M, m, s){
  temp<-sum(M^2*(1-m/M)*s/m)
  N*temp/n
}

a2<-srs_com2(250, 25, M, 20, sys1)
b2<-srs_com2(250, 25, M, 20, sys2)
c2<-srs_com2(250, 25, M, 20, sys3)

sqrt(a1+a2)/28462
sqrt(b1+b2)/28462
sqrt(c1+c2)/28462

srs_ratio_com1<-function(M,m,N,n,y_ra, mean_clu){
  Mbar<-sum(M)/n
  srs<-sum((M*(mean_clu-y_ra))^2)/(n-1)
  (1-n/N)*srs/(n*Mbar^2)
}

srs_ratio_com2<-function(N,n,M,m,s){
  temp<-sum(M^2*(1-m/M)*s/m)
  temp/(n*N*Mbar^2)
}

#ratio

yr1<-sum(ss$m_1/20*M)/sum(M)

yr2<-sum(ss$m_2/20*M)/sum(M)

yr3<-sum(ss$m_3/20*M)/sum(M)
yr1; yr2; yr3

a3<-srs_ratio_com1(M, 20, 250, 25, yr1, ss$m_1/20)
b3<-srs_ratio_com1(M, 20, 250, 25, yr2, ss$m_2/20)
c3<-srs_ratio_com1(M, 20, 250, 25, yr3, ss$m_3/20)

a4<-srs_ratio_com2(250, 25, M, 20, sys1)
b4<-srs_ratio_com2(250, 25, M, 20, sys2)
c4<-srs_ratio_com2(250, 25, M, 20, sys3)

sqrt(a3+a4)
sqrt(b3+b4)
sqrt(c3+c4)


ps$y1


ps<-read.csv("ps_sum1.csv", header=T, sep=",")
ps$sp

ps<-ps[,-c(2,3,7,8,9)]
ps
pr<-ps$size/k
pps_unb<-function(m, t, w2, pr){
  t_hat<-t*w2
  sum(t_hat/pr)/m
}

attach(ps)
t1_wr<-pps_unb(25, t1, size/20, pr)
t2_wr<-pps_unb(25, t2, size/20, pr)
t3_wr<-pps_unb(25, t3, size/20, pr)
t1_wr/k; t2_wr/k; t3_wr/k


pps_var<-function(m, w2,t_wr,prob, t){
  t_hat<-w2*t
  sum((t_hat/prob-t_wr)^2)/(m*(m-1))
}

v1<-pps_var(25,size/20, t1_wr,pr, t1)
v2<-pps_var(25,size/20, t2_wr,pr, t2)
v3<-pps_var(25,size/20, t3_wr,pr, t3)

sqrt(v1)/k
sqrt(v2)/k
sqrt(v3)/k

pps_ra<-function(m, prob, size, t_wr){
  k_wr<-sum(size/prob)/m
  t_wr/k_wr
}
y1_ra<-pps_ra(25, pr, size, t1_wr)
y2_ra<-pps_ra(25, pr, size, t2_wr)
y3_ra<-pps_ra(25, pr, size, t3_wr)

y1_ra; y2_ra; y3_ra

pps_ra_var<-function(m, prob, size,sm,mean){
  temp<-sum(((sm-mean)*size/prob)^2)
  k_wr<-sum(size/prob)/m
  temp/(m*(m-1)*k_wr^2)
}

sqrt(pps_ra_var(25, pr, size, (t1/20),99.9593045 ))
sqrt(pps_ra_var(25, pr, size, (t2/20), 7.0183919))
sqrt(pps_ra_var(25, pr, size, (t3/20), 0.5987633))




k<-read.csv("kkkk.csv", header=T, sep=",")

library(ggplot2)

k

plot(k[,2], type="l")
lines(k[,7], col="red")
lines(k[,4],col="green")
lines(k[,5], col="green")

plot(k[,1], type="l")
f<-1/sqrt(3.5-k[,2])
lines(f, col="red")

at<-read.csv("at.csv", header=T, sep=",")
at
at<-na.omit(at)

al1t<-read.csv("al1t.csv", header=T, sep=",")
al2t<-read.csv("al2t.csv", header=T, sep=",")
al2t<-na.omit(al2t)
al1t<-na.omit(al1t)
al1t
at
al2t
a1<-at$RESIDUAL
a2<-al1t$RESIDUAL
a3<-al2t$RESIDUAL

par(mfrow=c(1,1))
ccf(a1, a2)
ccf(a1, a3)

spr<-read.csv("spr.csv", header=T, sep=",")
econ<-read.csv("econn.csv", header=T, sep=",")

spr<-na.omit(spr)
econ<-na.omit(econ)

a4<-spr$RESIDUAL
a5<-econ$RESIDUAL

ccf(a4, a5)
ccf(a1, a2)
ccf(a1, a3)
a1
a2


oo<-read.csv("oo.csv", header=T, sep=",")
oo<-na.omit(oo)
a6<-oo$RESIDUAL

ccf(a3,a6)
