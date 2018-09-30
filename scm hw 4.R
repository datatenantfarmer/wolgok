setwd("/Users/chanheelee/Desktop/??????/CDA/raw data")

# Alligators and primary food choice
alli<-read.table("Alligators.txt", header=T)
alli$y<-relevel(factor(alli$y), ref="O")
library(VGAM)
model1<-vglm(y~x, family = multinomial(refLevel = "O"), data=alli)
summary(model1)
model1_1<-vglm(y~x, family = multinomial(refLevel="I"), data=alli)
summary(model1_1)
model1_2<-vglm(y~1, family = multinomial(refLevel="I"), data = alli)
summary(model1_2)
a<-deviance(model1_2)-deviance(model1_1)        
1-pchisq(a, df=2)
lines(fitted(model1_1)[,3]~alli$x)
lines(fitted(model1_1)[,1]~alli$x, col="red")
plot(fitted(model1_1)[,2]~alli$x, col="blue", type="l", ylim=c(0,1))

# Belief in afterlife
race<-rep(c("W", "B"), each=2)
gender<-rep(c("F", "M"),2)
yes<-c(371,250,64,25)
un<-c(49,45,9,5)
no<-c(74,71,15,13)

after<-data.frame(race, gender, yes, un, no)
after
model2<-vglm(cbind(yes, un, no)~race+gender, family = multinomial(refLevel = "no"), data=after)
summary(model2)
model2_1<-vglm(cbind(yes, un, no)~race, family = multinomial(refLevel = "no"), data=after)
a<-deviance(model2_1)-deviance(model2)
1-pchisq(a, df=2)
model2_2<-vglm(cbind(yes, un, no)~gender, family = multinomial(refLevel = "no"), data=after)
a1<-deviance(model2_2)-deviance(model2)
1-pchisq(a1, df=2)
fitted(model2)
summary(model2)

# political ideology
idea<-read.table("idea.txt", header=T)
model3<-vglm(cbind(very_liberal, liberal, moderate, conservative, very_conservative)~gender+party, family = cumulative(parallel = T), data=idea)
summary(model3)
model3_1<-vglm(cbind(very_liberal, liberal, moderate, conservative, very_conservative)~party, family = cumulative(parallel = T), data=idea)
1-pchisq(deviance(model3_1)-deviance(model3), df=1)
# working with model3_1 : delete gender effect
fitted(model3_1)
summary(model3_1)
model3_2<-vglm(cbind(very_liberal, liberal, moderate, conservative, very_conservative)~party, family = cumulative(parallel = FALSE), data=idea)
a2<-deviance(model3_1)-deviance(model3_2)
1-pchisq(a2, df=3)

# mental impairment
mental<-read.table("Mental.txt", header=T)
mental
model4<-vglm(impair~ses+life, family = cumulative(parallel = T), data=mental)
summary(model4)
model4_1<-vglm(impair~ses+life, family=cumulative(parallel = FALSE), data=mental)
a3<-deviance(model4)-deviance(model4_1)
1-pchisq(a3, df=4)

# Happiness
# baseline category logit model
income<-c(1,2,3)
not<-c(6,6,6)
pretty<-c(43,113,57)
very<-c(75,178,117)
happy<-data.frame(income, not, pretty, very)
happy
model5<-vglm(cbind(not, pretty, very)~income, family = multinomial(refLevel = "very"), data=happy)
summary(model5)
model5_1<-vglm(cbind(not, pretty, very)~1, family = multinomial(refLevel = "very"), data=happy)
a4<-deviance(model5_1)-deviance(model5)
1-pchisq(a4, df=2)
fit<-rbind(fitted(model5)[1,]*(6+43+75), fitted(model5)[2,]*(6+113+178), fitted(model5)[3,]*(6+57+117))
obs<-happy[,2:4]
val<-(obs-fit)^2/fit
1-pchisq(sum(val), df=4)
fitted(model5)

# cumulative model
model6<-vglm(cbind(not, pretty, very)~income, family = cumulative(parallel=T), data=happy)
summary(model6)
fit2<-rbind(fitted(model6)[1,]*(6+43+75), fitted(model6)[2,]*(6+113+178), fitted(model6)[3,]*(6+57+117))
val<-(obs-fit2)^2/fit2
1-pchisq(sum(val), df=4)
fitted(model6)
model6_1<-vglm(cbind(not, pretty, very)~1, family = cumulative(parallel=T), data=happy)
a<-deviance(model6_1)-deviance(model6)
1-pchisq(a, df=1)
# belief in afterlife 2
# log linear model : independence
race<-rep(c("W","B","O"), each=2)
res<-rep(c("yes", "no"), 3)
count<-c(1339,1639-1339,260,315-260,88,110-88)
after2<-data.frame(race, res, count)
after2$race<-relevel(factor(after2$race), ref="O")
after2$res<-relevel(factor(after2$res), ref="no")
model7<-glm(count~race+res, family = poisson, data=after2)
model7_1<-glm(count~race*res, family = poisson, data=after2)
summary(model7)
summary(model7_1)

# Alcohol & cigarette & marijuana use
alcohol<-rep(c("yes", "no"), each=4)
cigar<-rep(rep(c("yes", "no"), each=2),2)
mari<-rep(c("yes", "no"), 4)
count<-c(911, 538, 44, 456, 3, 43, 2, 279)
use<-data.frame(alcohol, cigar, mari, count)
colnames(use)<-c("A","C","M", "count")
model8<-glm(count~A+C+M, family = poisson, data=use)
summary(model8)
# homogeneity among variable
model8_1<-glm(count~A+C+M+A*C+A*M+C*M, family = poisson, data=use)
summary(model8_1)
val<-(count-fitted(model8_1))^2/fitted(model8_1)
1-pchisq(sum(val), df=1)
rstandard(model8_1)
model8_2<-glm(count~A+C+M+A*M+C*M, family = poisson, data=use)
anova(model8_2, model8_1, test="Chisq")

# car accident
acc<-read.table("Accidents.txt", header=T)
colnames(acc)<-c("g", "l", "s", "i", "count")
# independence model
model9<-glm(count~g+l+s+i, family=poisson, data=acc)
summary(model9)
# homogeneuous model
model9_1<-glm(count~g*l+g*s+g*i+l*s+l*i+s*i, family = poisson, data=acc)
summary(model9_1)
val<-(acc$count-fitted(model9_1))^2/fitted(model9_1)
sum(val)
1-pchisq(sum(val), df=1)
# 3 - factor model
model9_2<-glm(count~g*i*l+g*i*s+g*l*s+i*l*s, family=poisson, data=acc)
summary(model9_2)
val<-(acc$count-fitted(model9_2))^2/fitted(model9_2)
sum(val)
# reduced - 3 factor model
model9_3<-glm(count~g*l*s+g*i+i*l+i*s, family = poisson, data=acc)
summary(model9_3)
exp(-0.28274); exp(-0.28274+0.12858); exp(-0.20992)
val<-(acc$count-fitted(model9_3))^2/fitted(model9_3)
1-pchisq(sum(val), df=4)
fitted(model9_3)
fitted(model9_1)
dissimilar_index(sum(acc$count), model9_1, acc$count)
dissimilar_index(sum(acc$count), model9_3, acc$count)

# connection between logistic model and log - linear model
acc
yes<-c(973, 757, 996, 759,1084,513,812, 380)
no<-c(3246, 6134, 7287,11587,6123,6693,10381,10969)
g<-rep(c("F","M"), each=4)
l<-rep(rep(c("rural", "urban"), each=2),2)
s<-rep(c("no", "yes"), 4)
acc2<-data.frame(g, l, s, yes, no)
acc2
model9_4<-glm(cbind(yes, no)~g+l+s, family = binomial, data=acc2)
summary(model9_4)
summary(model9_3)
fitted(model9_3)
fitted(model9_4)

# Aids
G<-rep(c("M","F"), each=4)
I<-rep(rep(c("yes", "no"), each=2),2)
H<-rep(c("yes","no"), 4)
count<-c(76,160,6,25,114,181,11,48)
aids<-data.frame(G, I, H, count)
aids
model10<-glm(count~G*H+G*I+H*I, family=poisson, data=aids)
summary(model10)
fitted(model10)

val<-(aids$count-fitted(model10))^2/fitted(model10)
1-pchisq(sum(val), df=1)
exp(confint(model10)[6,])
model10_1<-glm(count~G*H+H*I, family = poisson, data=aids)
anova(model10_1, model10, test="Chisq")

exp(0.4636)
160*48/(25*181)
fitted(model10)

u<-runif(1e+5,0,2*pi)
mean(exp(cos(u)))
u1<-runif(1e+5)
mean(exp(cos(2*pi*u1)))

pf(3,3,2)

# 1
# unif(0,1)
g<-function(x) exp(-x)/(1+x^2)
U<-runif(1e+5)
val1<-g(U)
mean(val1)
var(val1)

# exp(1)
exp.gen<-function(lambda,n){
  u<-runif(n)
  x<--log(u)/lambda
  return(x)
}
x1<-exp.gen(1, 1e+5)
h1<-function(x) exp(-exp(-x))/(1+exp(-2*x))
val2<-h1(x1)
var(val2)
mean(val2)

# cauchy
cauchy.gen<-function(n){
  u<-runif(n)
  x<-tan(pi*(u-0.5))
  return(x)
}

x2<-cauchy.gen(1e+5)
x2_I<-numeric(1e+5)
for (i in 1:1e+5){
  if (x2[i]>=0 && x2[i]<=1){
    x2_I[i]<-x2[i]
  } else {
    x2_I[i]<-0
  }
}
t<-function(x) pi*exp(-x)
x2_I
I<-numeric(1e+5)
for (i in 1:1e+5){
  if (x2_I[i]==0){
    I[i]<-0
  }else{
    I[i]<-1
  }
}
val3<-I*t(x2_I)

# truncated exp
truncated.exp<-function(n){
  u<-runif(n)
  x<--log(1-u*(1-exp(-1)))
}

x3<-truncated.exp(1e+5)
val4<-(1-exp(-1))/(1+x3^2)
var(val4)
mean(val4)

# truncated cauchy
truncated.cauchy<-function(n){
  u<-runif(n)
  x<-tan(pi*u/4)
  return(x)
}

x4<-truncated.cauchy(1e+5)
val5<-pi*exp(-x4)/4
var(val5)
mean(val5)

comparison<-data.frame(val1, val2, val3, val4, val5)
est<-sapply(comparison, mean)
est.var<-sapply(comparison, var)
com<-rbind(est, est.var)
rownames(com)<-c("Estimate", "Variance")
colnames(com)<-c("Unif(0,1)", "Exp(1)", "Cauchy(0,1)", "truncated.exp", "truncated.cauchy")
com

# F distribution
norm.gen<-function(n){
  theta<-runif(n)*2*pi
  t<-exp.gen(1/2, n)
  z<-sqrt(t)*cos(theta)
  return(z)
}

F.gen<-function(m,n){
  v1<-sum(norm.gen(m)^2)/m
  v2<-sum(norm.gen(n)^2)/n
  return(v1/v2)
}

r<-replicate(1e+5, F.gen(20,9))
var(r)
mean(r>15)


# Impotance sampling
u<-runif(1e+5)
x<-15/((1-u)^(2/9))

target<-function(m,n,x){
  m^(m/2)*n^(n/2)*x^(m/2-1)*(x^(n/2+1))/((m*x+n)^((m+n)/2)*beta(0.5*m, 0.5*n)*(n/2)*15^(n/2))
}
value<-target(20,9,x)
var(value)
mean(value)

# 3
pmf<-matrix(c(0.01,0.39,0.11,0.18,0.26,0.05), 1, 6)
colnames(pmf)<-c("1","2","3","4","5","6")
rownames(pmf)<-c("P(X=x)")
qmat<-matrix(rep(1/6, 36), 6,6)
qmat
metro.dis<-function(n,init,pmf,qmat){
  x<-rep(init,n)
  for (i in 2:n){
    y<-sample(1:dim(qmat)[1], 1, prob=qmat[x[i-1],])
    alpha<-min(1, pmf[y]/pmf[x[i-1]])
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

pmf
rnd<-metro.dis(1e+5,sample(1:6,1),pmf, qmat)
rnd_new<-burn_thin(100, 80, 200, rnd)
ts.plot(rnd_new)
acf(rnd_new)

# 4
metrot<-function(n,v,sig,init){
  x<-rep(init,n)
  f<-function(x) (1+x^2/v)^(-(v+1)/2)
  for (i in 2:n){
    y<-sqrt(sig)*norm.gen(1)+x[i-1]
    alpha<-f(y)/f(x[i-1])
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}
t1<-metrot(1e+5, 4,0.1, -5)
burn_thin<-function(a,b,n,x){
  x1<-x[a:length(x)]
  k<-1
  xnew<-x1[k]
  for (i in 1:length(x)){
    xnew<-c(xnew,x1[b*i])
    if (length(xnew)==n) break
  }
  return(xnew)
}
t1_new<-burn_thin(400, 80, 1000, t1)
hist(t1_new, prob=T)
acf(t1_new)
ts.plot(t1_new)
e<-dt(x, df=4)
lines(e~x)

t2<-metrot(1e+5, 4, 1, -5)
t2_new<-burn_thin(400, 80, 1000, t2)
hist(t2_new, prob=T)
acf(t2_new)
ts.plot(t2_new)
e<-dt(x, df=4)
lines(e~x)

t3<-metrot(1e+5, 4, 2, -5)
t3_new<-burn_thin(400, 80, 1000, t3)
hist(t3_new, prob=T)
acf(t3_new)
ts.plot(t3_new)
e<-dt(x, df=4)
lines(e~x)

t4<-metrot(1e+5, 4, 10, -5)
t4_new<-burn_thin(400, 80, 1000, t4)
hist(t4_new, prob=T)
acf(t4_new)
ts.plot(t4_new)
e<-dt(x, df=4)
lines(e~x)

metrot_cauchy<-function(n,v,init){
  x<-rep(init,n)
  f<-function(x) (1+x^2/v)^(-(v+1)/2)
  for (i in 2:n){
    y<-cauchy.gen(1)
    alpha<-f(y)/f(x[i-1])
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

t5<-metrot_cauchy(1e+5, 4, -5)
hist(t5, prob=T)
t5_new<-burn_thin(400, 80, 1000, t5)
hist(t5_new, prob=T)
acf(t5_new)
ts.plot(t5_new)
x<-seq(-6,6,0.001)
e<-dt(x, df=4)
lines(e~x)


# 4. Gibbs sampler

metro.gen<-function(n, init,a,sig){
  x<-rep(init, n)
  for (i in 2:n){
    y<-sqrt(sig)*norm.gen(1)+x[i-1]
    if (y>0){
      alpha<-min(1,exp(-a*y-y)/exp(-a*x[i-1]-x[i-1]))
    }else{
      alpha<-0
    }
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

gib.gen<-function(N,n,init){
  out<-matrix(rep(init, 2*N), nrow=N, ncol=2)
  for (i in 2:N){
    x1.tmp<-metro.gen(n,out[i-1,2],init, 0.1)
    out[i,1]<-x1.tmp[sample(floor(n/2):n, 1)]
    x2.tmp<-metro.gen(n,out[i,1],init, 0.1)
    out[i,2]<-x2.tmp[sample(floor(n/2):n, 1)]
  }
  return(out)
}

gib<-gib.gen(4000, 200, 3)
gib
apply(gib, 2, mean)
apply(gib, 2, min)
mean(gib[,1])
mean(gib[,2])

f<-function(x,y) exp(-x*y-x-y)
x<-seq(0,5,0.1)
y<-x
z<-outer(x,y,f)
contour(x,y,z, levels = c(0.005, 0.01,0.03, 0.05, 0.1, 0.15, 0.5), xlim=c(0,4.5), ylim=c(0,4.5))
points(gib[,1],gib[,2],col="green")
persp(x,y,z)
x<-seq(-10,10, 0.01)
t<-function(x) dt(x,df=4)
i<-function(x) x*exp(-x)/((x+1)*0.5963474)
integrate(i, 0, Inf)

par(mfrow=c(3,2))
ts.plot(t1[1:1500], main="Normal proposal with variance 0.1")
ts.plot(t2[1:1500], main="Normal proposal with variance 1")
ts.plot(t3[1:1500], main="Normal proposal with variance 2")
ts.plot(t4[1:1500], main="Normal proposal with variance 10")
ts.plot(t5[1:1500], main="Cauchy proposal")
ts.plot(rnd[1:1000])
acf(t1_new)

qchisq(0.95, df=1)

ara<-function(a){
  f<-function(x) exp(-a)*exp(-x*(a+1))
  g<-function(x) exp(-x)
  M<-optimize(i<-function(x) f(x)/g(x), c(0,100),maximum = T)$object
  while(TRUE){
    xstar<-exp.gen(1,1)
    u<-runif(1)
    if (u<f(xstar)/(M*g(xstar))){
      return(xstar)
    }
  }
}

gib.gen2<-function(N, init){
  out<-matrix(rep(init, N*2), nrow=N, ncol=2)
  for (i in 2:N){
    out[i, 1]<-ara(out[i-1,2])
    out[i, 2]<-ara(out[i,1])
  }
  return(out)
}


burn_thin.mat<-function(a,b,n,mat){
  mat1<-mat[a:dim(mat)[1],]
  k<-1
  matnew<-mat1[k]
  for (i in 1:dim(mat)[1]){
    matnew<-rbind(matnew,mat1[b*i,])
    if (dim(matnew)[1]==n) break
  }
  return(matnew)
}

gib<-gib.gen2(1e+4,0.5)
apply(gib, 2, mean)
gib_new<-burn_thin.mat(1000, 1, 1000, gib)
apply(gib_new, 2, mean)

f<-function(x,y) exp(-x*y-x-y)
x<-seq(0,5,0.1)
y<-x
z<-outer(x,y,f)
contour(x,y,z, levels=c(0.001, 0.005, 0.01, 0.03,0.05, 0.1, 0.15, 0.3, 0.5,1), xlim=c(0,4.5), ylim=c(0,4.5), col="red")
points(gib_new[,1],gib_new[,2],col="green")

qnorm(0.025)
qf(0.1, 1,2)
1/qf(0.9, 2,1)
1-ppois(7, 4)
1-ppois(9, 4)
pnorm((22.5-25)/(3/2))
pnorm((22.5-23)/(3/2))

a<-c(22, 24.5, 23, 26.5)
mean(a)
pnorm((24-25)/(3/2))
qchisq(0.9, 24)
pnorm(2/(sqrt(5/3)))

A<-matrix(c(1,2,1,2,2,1,1,2,3), byrow=T, 3,3)
b<-matrix(c(5,6,9), nrow=3)
solve(A, b)
