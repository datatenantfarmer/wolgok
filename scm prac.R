# Monte carlo integral
U<-runif(1e+4)
mean(U^2)
mean(exp(-U))
1-exp(-1)
U<-runif(1e+4, 0, 2*pi)
2*pi*mean(exp(cos(U)))

# random number generating
x<-replicate(1e+4, max(runif(10)))
hist(x, prob=T)
n<-1000
theta<-2*pi*runif(n)
x<-sin(theta)
hist(x, prob=T)

n<-1e+4
exp.gen<-function(lambda, u){
  x<--log(u)/lambda
  return(x)
}
u<-runif(n)
x<-exp.gen(1/3, u)
hist(x, prob=T)
x1<-rexp(10000, 1/3)
hist(x1, prob=T, add=T)

beta.s1.gen<-function(alpha, u){
  x<-u^{1/alpha}
  return(x)
}
x<-beta.s1.gen(3, u)
hist(x, prob=T)
x1<-rbeta(n,3, 1)
hist(x1, prob=T, add=T)

# discrete case : inverse trandformation
bin.sim<-function(n,p){
  count<-0; px<-(1-p)^n; Fx<-px; u<-runif(1)
  while(Fx<u){
    count<-count+1
    px<-px*p*(n-count+1)/((1-p)*count)
    Fx<-Fx+px
  }
  return(count)
}
bin.sim(10, 0.4)
x<-replicate(1e+4,bin.sim(10, 0.4))
hist(x, prob=T)
x1<-rbinom(1e+4, 10, 0.4)
hist(x1, prob=T, add=T)

x<-exp.gen(1/3, u)
x<-sqrt(x)
hist(x, prob=T)
f<-function(x, lam){
  2*lam*x*exp(-x^2*lam)
}
x<-seq(0,5,0.01)
x1<-f(x, 1/3)
lines(x1~x, col="red")

gam.sim<-function(n, lam){
  x<-replicate(n, exp.gen(lam, runif(1)))
  return(sum(x))
}

x<-replicate(1e+4, gam.sim(3, 1/3))
hist(x, prob=T)

x1<-rgamma(1e+4, 3, 1/3)
hist(x1, prob=T, add=T)

laplace.sim1<-function(lam, n){
  u1<-runif(n); u2<-runif(n)
  x<-exp.gen(lam, u1)
  y<-exp.gen(lam, u2)
  return(x-y)
}

x<-laplace.sim1(1/3, 1e+4)
hist(x, prob=T)

x1<-sample(c(-1,1), 1e+4, replace = T)*exp.gen(1/3, runif(1e+4))
hist(x1, prob=T, add=T)

# geometric random variable : # of trials to get first sucess
X<-1; p<-0.4; suc<-FALSE
while(!suc){
  u<-runif(1)
  if (u<p){
    suc<-TRUE
  } else {
    X<-X+1
  }
}

# geometric random variable : # of failure to get first sucess
geo.sim<-function(p){
  px<-p; Fx<-px; U<-runif(1); x<-0
  while (Fx<U){
    x<-x+1
    px<-px*(1-p)
    Fx<-Fx+px
  }
  return(x)
}
geo.sim(0.4)
r<-replicate(1e+4, geo.sim(0.4))
hist(r, prob=T)
r1<-rgeom(1e+4, 0.4)
hist(r1, prob=T, add=T)

nb.sim<-function(r, p){
  t<-replicate(r, geo.sim(p))
  out<-sum(t)
  return(out)
}
nb.sim(3, 0.4)
r<-replicate(1e+4, nb.sim(3, 0.4))
hist(r, prob=T)
r2<-rnbinom(1e+4, 3, 0.4)
hist(r2, prob=T, add=T)

# coupon collector's problem
my.card<-sample(1:12, 20, replace=T)
length(unique(my.card))
table(my.card)

collector<-function(n,N){
  my.card<-sample(1:N, n, replace = T)
  uni<-length(unique(my.card))
  ifelse(uni==N,1,0)
}
coupon<-replicate(1e+4,collector(20,12))
table(coupon)/1e+4

collector2<-function(n){
  count<-1
  my.card<-sample(1:n, 1, replace=T)
  uni<-length(unique(my.card))
  while(uni<n){
    sam.card<-sample(1:n,1,replace=T)
    my.card<-c(my.card, sam.card)
    uni<-length(unique(my.card))
    count<-count+1
  }
  return(count)
}
r<-replicate(1e+4, collector2(12))
mean(r)

poi.gen<-function(lambda){
  time<-0
  count<-0
  while(time<1){
    time<-time+exp.gen(lambda, runif(1))
    count<-count+1
  }
  return(count-1)
}
r<-replicate(1e+4,poi.gen(4))
mean(r)

gen.poi<-function(lambda, at){
  x<-exp.gen(lambda, runif(1000))
  if (sum(x)<at){
    return(NA)
  } 
  if (x[1]>at){
    return(0)
  } else {
    return(max(which(cumsum(x)<at)))
  }
}
r<-replicate(1e+4,gen.poi(3,2))
mean(r)

mytime<-40; lam<-1/20; waitingtime<-numeric(1000)
for (i in 1:1000){
  arrival<-exp.gen(lam, runif(1))
  wait<-arrival-mytime
  while(wait<0){
    arrival<-arrival+exp.gen(lam, runif(1))
    wait<-arrival-mytime
  }
  waitingtime[i]<-wait
}
mean(waitingtime)

A<-numeric(1e+4); B<-numeric(1e+4)
for (i in 1:1e+4){
  bus<-exp.gen(lam, runif(1))
  A[i]<-bus
  while (bus<10){bus<-bus+exp.gen(lam, runif(1))}
  B[i]<-bus-10
}
mean(A)
mean(B)

R2<-exp.gen(1/2, runif(1000))
R<-sqrt(R2)
theta<-2*pi*runif(1000)
x<-R*cos(theta)
y<-R*sin(theta)
hist(x, prob=T)
hist(y, prob=T, add=T)

nsim<-10^3; n<-4; 
x <-matrix(rnorm(n*nsim,1,2),nrow=nsim, ncol=n)
sx <- apply(x,1,sd); tn<-sqrt(n)*(rowMeans(x)-1)/sx
hist(tn, prob=T)

tgen2<-function(n){
  v<-sum(rnorm(n,0,1)^2)
  tn<-rnorm(1,0,sqrt(n/v))
  return(tn)
}
tn<-replicate(1e+4, tgen2(3))
hist(tn, prob=T)

xstar<-2*runif(1000)
X<-xstar[runif(1000)<1-abs(1-xstar)]
hist(X, prob=T)

rej1<-function(f,a,b,M){
  while(TRUE){
    x<-runif(1,a,b); u<-runif(1,0,M)
    if (u<f(x)){
      return(x)
    }
  }
}
f<-function(x){
  if (x>0 && x<2){
    1-abs(1-x)
  }
}
x<-numeric(3000)
for (i in 1:3000){
  x[i]<-rej1(f,0,2,1)
}
hist(x, prob=T)

f<-function(x){
  x^1.7*(1-x)^5.3/beta(2.7, 6.3)
}
M<-optimize(f, c(0,1), maximum = T)$objective

xstar<-runif(3000)
u<-runif(3000, 0, M)
x<-xstar[u<f(xstar)]
plot(xstar, u)
u1<-u[u<f(xstar)]
points(u1~x, col="red")
z=seq(0,1,0.001)
lines(z,f(z),lwd=2,col=3)

g<-function(x){
  x*(1-x)^5/beta(2,6)
}

beta.sim<-function(f, g, a, b){
  while(TRUE){
    gam1<-sum(exp.gen(1,runif(a)))
    gam2<-sum(exp.gen(1,runif(b)))
    xstar<-gam1/(gam1+gam2)
    M<-optimize(i<-function(x) f(x)/g(x), maximum = T, c(0,1))$objective
    U<-runif(1)
    if (U<f(xstar)/(M*g(xstar))){
      return(xstar)
    }
  }
  return(xstar)
}
r<-replicate(1e+4,beta.sim(f,g,2,6))
hist(r, prob=T)
lines(f(z)~z, col="red")

f<-function(x){
  2/sqrt(pi)*x^0.5*exp(-x)
}
g<-function(x){
  2/3*exp(-2*x/3)
}

gam.sim<-function(lam){
  while(TRUE){
    xstar<-exp.gen(lam, runif(1))
    M<-optimize(i<-function(x) f(x)/g(x), c(0, 100), maximum = T)$objective
    U<-runif(1)
    if (U<f(xstar)/(M*g(xstar))){
      return(xstar)
    }
  }
}  
r<-replicate(1e+4,gam.sim(2/3))
hist(r, prob=T)
x<-seq(0, 12, 0.01)
lines(f(x)~x, col="red")

f<-function(x){
  2/sqrt(2*pi)*exp(-x^2/2)
}
g<-function(x){
  exp(-x)
}

norm.gen<-function(lam){
  while(TRUE){
    xstar<-exp.gen(lam, runif(1))
    U<-runif(1)
    M<-optimize(i<-function(x) f(x)/g(x), c(0,100), maximum = T)$objective
    if(U<f(xstar)/(M*g(xstar))){
      S<-sample(c(-1,1),1, replace=T)
      return(xstar*S)
  }
  }
}

r<-replicate(1e+4, norm.gen(1))
hist(r, prob=T)

f<-function(x) 2^100*x^200
x<-seq(-0.1,0.1, 0.001)
plot(f(x)~x, col="red")

pbinom(12, 20, 0.4)
pbinom(12, 20, 0.5)
pbinom(12, 20, 0.6)
pbinom(12, 20, 0.7)
1.1-0.95

-qnorm(0.05, 0,1)/2+7

a<-(7.82-7.5)/sqrt(5/20)
1-pnorm(a, 0, 1)

b<-(7.82-9)/sqrt(5/20)
b
1-pnorm(b, 0, 1)
qnorm(0.8, 0,1)
pnorm(0.8416, 0,1)
5/(0.18/0.841621)^2
qnorm(.025, 0,1)
(1.96*2)^2

theta<-seq(0, 10, 0.001)
f<-function(x) 1-.5^theta
plot(f(theta)~theta, type="l")
7/27
6/27
20/27
19/27
1/27

1-ppois(7, 4)
1-0.949
8*.75

1-ppois(7,6)
1-ppois(7,8)
1-ppois(7,8*1.25)
1-ppois(3,4)
1-ppois(9,4)
a<-(22.5-25)/sqrt(9/4)
pnorm(a, 0,1)
a<-(22.5-23)/sqrt(9/4)
pnorm(a,0,1)

a<-22+24.5+23+26.5
a/4
(a/4-25)/sqrt(9/4)
pnorm(-0.67, 0,1)

256/800

u<-runif(1e+5)
up<-0.9*u+0.1
.9*mean(log(up)^2)

u<-runif(1e+5,.1,1)
.9*mean(log(u)^2)
gamma.gen<-function(alpha, lambda){
  r<-exp.gen(lambda, alpha)
  x<-sum(r)
  return(x)
}

r<-replicate(1e+5, gamma.gen(3,1))
2*mean(r<log(10))

u<-2*runif(1e+5)-1
v<-2*runif(1e+5)-1
r2<-u^2+v^2
mean(r2<1)*4

buffon<-function(l){
  theta<-pi*runif(1e+5)/2; d<-l*runif(1e+5)/2
  r<-l*cos(theta)/2
  return(mean(d<r))
}
1/buffon(1)*2

f<-function(x) x^3-7*x^2+1
d<-optimize(f, c(0,1), maximum = T)$objective
c<-optimize(f, c(0,1))$objective

u<-runif(1e+5)
hm<-function(f, a, b, c, d, n){
  u<-(b-a)*runif(n)+a
  v<-(d-c)*runif(n)+c
  out<-mean(v<=f(u))
  I<-out*(b-a)*(d-c)+c*(b-a)
  return(I)
}
hm(f, 0, 1, -5, 1, 1e+5)

mc.int<-function(f, a, b, n){
  u<-runif(n)
  I<-mean(f(u))*(b-a)
  return(I)
}
mc.int(f, 0, 1, 1e+5)

u<-runif(1e+5)
f<-function(x){
  1/sqrt(2*pi)*exp(-x^2/2)
}
.5-1.96*mean(f(1.96*u))

r<-norm.gen(1e+5)
mean(r>1.96)

mean(1-u^2)

ara<-function(f){
  while(TRUE){
    u<-runif(1)
    v<-2*runif(1)
    if (v<=f(u)){
      return(u)
    }
  }
}
f<-function(x) 2*(1-x)
r<-replicate(1e+5, ara(f))
mean((1+r)/2)

u<-runif(1e+5)
x<-(u/16)^(-.25)
val<-(2+x)^1.5/64
mean(val)
f<-function(x) (2+x)^1.5/x^5
integrate(f, 2, Inf)

.5-mean(f(4.5*u))*4.5

r<-norm.gen(1e+5)
mean(r>4.5)

u<-exp.gen(1, 1e+5)+4.5
mean(f(u)/exp(-u+4.5))

sqrt(2*pi)*mean(r>0 & r<1)
a<-pnorm(1)-0.5
sqrt(2*pi)*a

xgen<-function(n){
  u<-runif(n)
  x<-sqrt(2)*tan(u*atan(1/sqrt(2)))
  return(x)
}
x<-xgen(1e+5)

val<-exp(-x^2/2)*sqrt(2)*atan(1/sqrt(2))*(1+x^2/2)
mean(val)
var(val)

f<-function(x) exp(-x)*x^2/2
integrate(f, 0, 1)
integrate(f, 0, 2/0.1)

qchisq(0.95, 1)
