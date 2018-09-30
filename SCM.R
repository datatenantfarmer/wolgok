# Calculate factorial in r : inf 
# Using built in fuction : choose(n,k)

choose(200,100)

# Using 'lfactorial'
exp(lfactorial(200)-2*lfactorial(100))

# Using loop
a<-1
for (i in 1:100){
  a <- a*(100+i)/i
}
a

# built in function 'prod'
prod(101:200)/prod(1:100)

# birthday problem
# Using loop
a<-1
for (i in 1:50){
  a <- a*(366-i)/365
}
1-a

# Using built in function 'prod'
1-prod((366-50):365)/365^50

# Using 'lfactorial'
1-exp(lfactorial(365)-lfactorial(365-50)-50*log(365))

# Using built in function 'pbirthday, qbirthday'
pbirthday(50)
qbirthday(0.5)

# Define User defined function : birthday problem
pbirth<-function(n){
  return(1-exp(lfactorial(365)-lfactorial(365-n)-n*log(365)))
}
plot((1:100),pbirth(1:100))

# Monte - carlo simulation : n = 25
r<-replicate(1e+4, max(tabulate(sample(1:365, 25, replace=T))))
mean(r>=2)
pbirth(25)

# Taylor expansion : exp
expo<-function(x){
  sum<-1; temp<-1; k<-1
  while (abs(temp)>.Machine$double.eps){
    temp <-temp * x/k
    sum<-sum + temp
    k<-k+1
  }
  return(sum)
}
print(expo(1), 16)
print(exp(1), 16)

# cumputation of exponential function : Taylor expansion and Euler number
# what is exp(100)?
# Using taylor expansion
expo1<-function(x){
  sum<-1; temp<-1
  for (i in 1:100){
    if (abs(temp)<.Machine$double.eps) return(sum)
    temp<-temp*x/i
    sum<-sum+temp
  }
  print("computation is incomplete within the limit 100")
}
expo1(100)
expo1(1)^100

# Using Euler number
diff<-0.5; k<-1
while(abs(diff)>.Machine$double.eps){
  temp<-(1+1/k)^k
  k<-k+1
  val<-(1+1/k)^k
  diff <- val - temp
}
print(val, 16)
e<-sum(1/factorial(0:100))

# Talyor expansion : ln(1.5)? ---> More effective code is needed
a<-proc.time()
eps<-1e-12
x<-0.5
log1x<-0
n<-0
while (n==0 || abs(last.term)>eps){
  n<-n+1
  last.term<-(-1)^(n+1)*x^n/n
  log1x<-log1x+last.term
}
log1x
log(1+0.5)
proc.time() - a

# More effective one
a<-proc.time()
eps<-1e-12
n<-1
x<-0.5
log1x<-x
last.term<-x
while (abs(last.term)>eps){
  n<-n+1
  last.term <- -last.term*x*(n-1)/n
  log1x <- log1x + last.term
}
log1x
proc.time() - a

# vectorization is more effective than loop

# Using loop
sum<-0
nobs<-1e+5
for (i in 1:nobs){
  x<-rnorm(1,0,1)
  y<-rnorm(1,0,1)
  sum<-sum+max(x,y)
}
sum/nobs

# To achieve efficiency, Using matrix
A<-matrix(rnorm(2*nobs, 0,1), ncol=2)
maxval<-pmax(A[,1], A[,2])
mean(maxval)

# Monte-carlo simulation to solve bead problem
sim1<-function(n){
  nb1<-10; n1<-18; n2<-13; count<-0
  for (i in 1:n){
    nb2<-6
    if (runif(1)<nb1/n1) nb2<-nb2+1
    if (runif(1)<nb2/n2) count<-count+1
  }
  return(count/n)
}
sim1(1000)

sim2<-function(n){
 snd<-numeric(n)
 for (i in 1:n){
   x<-sample(c(0,1), 1, prob=c(8/18, 10/18))
   if (x==1) pb<-7/13 else pb<-6/13
   snd[i]<-sample(c(1,0), 1, prob=c(pb, 1-pb))
 }
 return(mean(snd))
}
sim2(100000)

# Using matrix to achieve efficiency
# Using double loop
A<-matrix(runif(1e+5), nrow=1000)
rowsums<-rep(0, 1000)

for (i in 1:1000){
  s<-0
  for (j in 1:100){
    s<-s+A[i,j]
  }
  rowsums[i]<-s
}
rowsums

# Using only one loop
A
for (i in 1:1000){
  rowsums[i]<-sum(A[i,])
}
rowsums

# Using built in function 'apply'
apply(A, 1, sum)

# Column's minimum
c<-rep(100,100)
for (j in 1:100){
  for (i in 1:1000){
    if (c[j]>A[i,j]) c[j]<-A[i,j]
  }
}

apply(A,2,min)

for (i in 1:100){
  c[i]<-min(A[,i])
}
c

# Sequence and reccurence relation
# Stirling's approximation
q<-function(n) sqrt(2*pi*n)*(n/exp(1))^n
factorial(100)/q(100)

q1<-function(n){
  log(sqrt(2*pi*n))+n*(log(n)-1)
}
exp(lfactorial(1000)-q1(1000))

# Fibonacci sequence
fibo<-function(n, x1,x2){
  temp1<-x1
  temp2<-x2
  for (i in 3:n){
    temp3<-temp1+temp2
    temp1<-temp2
    temp2<-temp3
  }
  return(temp3)
}
fibo(10,1,1)

# Recursive version of fibonacci sequence
fibo1<-function(n,x1,x2){
  if (n>=3){
    term<-fibo1(n-1,x1,x2)+fibo1(n-2,x1,x2)
  } else {
    term<-ifelse(n==1, x1,x2)
  }
  return(term)
}
fibo1(10,1,1)
F<-c(1,1); n<-1
while(F[n]<=100){
  n<-n+1
  F[n+1]<-F[n]+F[n-1]
}
F

n<-50; F<-rep(0,n); F[1]<-1; F[2]<-1
for (i in 3:50){
  F[i]<-F[i-1]+F[i-2]
}
F

# Fibonacci sequence follows benford's law?
n<-1000; F<-rep(0,n); F[1]<-1; F[2]<-1
for (i in 3:n){
  F[i]<-F[i-1]+F[i-2]
}
F
table(F%/%10^floor(log(F,10)))

# Match problem
n<-50; a<-0
for (i in 1:n){
  a<-a+(-1)^(i+1)/factorial(i)
}
a

# Match problem : recurrence relation
n<-10; pb<-rep(0,n); pb[1]=0; pb[2]<-0.5
for (i in 3:10){
  pb[i]<-(i-1)/i*pb[i-1]+1/i*pb[i-2]
}
pb

# Match problem : Monte carlo simulation
n<-50
r<-replicate(10^4, sum(sample(n)==1:50))
mean(r>=1)

# Gambler's ruin : Using recurrence relation
N<-10; pb<-rep(0,N+1); pb[1]<-0; pb[2]<-0.1
for (i in 2:N){
  pb[i+1]<-2*pb[i]-pb[i-1]
}
pb

# Gambler's ruin : Using Monte carlo simulation
ruin<-function(k,N,p){
  amount<-k
  while(amount>0 & amount < N){
    bet<-sample(c(1,-1),1,prob=c(p,1-p))
    amount<-amount+bet
  }
  if (amount==0) return(1) else return(0)
}
ruin(1,10,0.5)

r<-replicate(10^4, ruin(1,10,0.5))
mean(r)

# LCS for LCG
lcg<-function(n,m,a,c,x0){
  X<-numeric(n)
  xn<-x0
  for (i in 1:n){
    xn<-(a*xn+c)%%m
    X[i]<-xn
  }
  return(X)
}
lcg(10,8,5,1,0)

# LCG
myrand<-numeric(30268)
raseed<-27218
for (i in 1:30268){
  raseed<-(171*raseed)%%30269
  myrand[i]<-raseed/30269
}
myrand[1:60]
length(unique(myrand))

# Define User defined function : myrng
myrng<-function(n,a,c,m,x0){
  x<-numeric(n)
  x[1]<-x0
  for (i in 1:n){
    x[i+1]<-(a*x[i]+c)%%m
  }
  return(x[1:n]/m)
}

x<-myrng(1000,171,0,30269,27218)
# Testing Uniformity of lcg : chi squared test
rng.chi.test<-function(x,m){
  obs<-table(trunc(x*m)/m)
  exp<-length(x)*rep(1,m)/m
  chival<-sum((obs-exp)^2/exp)
  pval<-1-pchisq(chival, m-1)
  return(list(test.stat=chival, p.value=pval, df=m-1))
}
rng.chi.test(x, 10)
u<-runif(1000)
ks.test(x,u)

# Testing independence
plot(x[2:1000]~x[1:999])
lag.plot(x)
acf(x)

# Fixed point iteraction : x = f(x)
n<-50; x<-rep(0,n); x[1]<-0.5; diff<-0.5
eps<-0.00005; k<-1
while(diff>eps){
  k<-k+1
  x[k]<-cos(x[k-1])
  diff<-abs(x[k]-x[k-1])
}
k
x[25]

f<-function(x) -0.5*x^3+x^2+1

x<-rep(0,n)
x[1]<-0.5
diff<-0.5
k<-1
while(diff>eps){
  k<-k+1
  x[k]<-f(x[k-1])
  diff<-abs(x[k]-x[k-1])
}
k
x[k]

fixed.point<-function(ftn,x0, tol=1e-9, max.iter=100){
  xold<-x0
  xnew<-ftn(xold)
  iter<-1
  cat("At iteraction 1, value of x is:", xnew, "\n")
  while((abs(xnew-xold)>tol) && (iter < max.iter)){
    xold<-xnew
    xnew<-ftn(xold)
    iter<-iter+1
    cat("At ieraction", iter, "value of x is:", xnew, "\n")
  }
  if ((abs(xnew-xold))>tol){
    cat("Algorithm fails to converge\n")
    return(NULL)
  } else {
    return(xnew)
  }
}
g<-function(x) exp(exp(-x))

fixed.point(g,2)

# Newton - Raphson method
newtonraphson<-function(fx,fpx,x0,eps,max.iter){
  diff<-1; x<-x0; iter<-0
  while((abs(diff)>eps) && (iter<max.iter)){
    diff<--fx(x)/fpx(x)
    x<-x+diff
    iter<-iter+1
  }
  return(x)
}

fx<-function(x) x^5-8*x^2+3
fpx<-function(x) 5*x^4-16*x
newtonraphson(fx,fpx,0.4,0.00001, 100)

# Using built in function 'uniroot'
uniroot(fx, c(0,1), tol=1e-5)$root

# Linear aljebra 
rowscale<-function(m,row,k){
  m[row,]<-k*m[row,]
  return(m)
}

rowswap<-function(m,row1,row2){
  temp<-m[row1,]
  m[row1,]<-m[row2,]
  m[row2,]<-temp
  return(m)
}

rowtransform<-function(m, row1,row2,k){
  m[row2,]<-m[row2,]+k*m[row1,]
  return(m)
}

A <- matrix(c(2,-5,4,1,-2,1,1,-4,6), byrow=T, nrow=3, ncol=3)
b <- matrix(c(-3,5,10),nrow=3,ncol=1)
p <- nrow(A); U.pls <- cbind(A,b)

# Using naive method
for (i in 1:p){
  for (j in (i+1):(p+1)) U.pls[i,j]<-U.pls[i,j]/U.pls[i,i]
  U.pls[i,i]<-1
  if (i <p){
    for (k in (i+1):p){
      U.pls[k,]<-U.pls[k,]-U.pls[k,i]/U.pls[i,i]*U.pls[i,]
    }
  }
}
U.pls

# Using user defined function
for (i in 1:(p-1)){
  for (j in (i+1):p){
    k<--U.pls[j,i]/U.pls[i,i]
    U.pls<-rowtransform(U.pls,i,j,k)
  }
}
U.pls

# find rref
for (i in 1:(p-1)){
  for (j in (i+1):p){
    k<--U.pls[j,i]/U.pls[i,i]
    U.pls<-rowtransform(U.pls,i,j,k)
    U.pls<-rowscale(U.pls,j,1/U.pls[j,j])
  }
  U.pls<-rowscale(U.pls,i,1/U.pls[i,i])
}
U.pls

# back - substitution
for (i in (p-1):1){
  for (j in i:1){
    k<--U.pls[j,i+1]
    U.pls<-rowtransform(U.pls,i+1,j,k)
  }
}
U.pls

# Using rowswap, 0 in pivot diagonal
A <- matrix(c(0,-5,4,1,-2,1,1,-4,6),byrow=T,nrow=3,ncol=3)
b <- matrix(c(-3,5,10),nrow=3,ncol=1)
p <- nrow(A); eps <- 0.001
U.pls <- cbind(A,b)
eps<-0.001
rowreduc1<-function(U.pls){
  p<-nrow(U.pls)
  for (i in 1:p){
    if ((abs(U.pls[i,i]) < eps) & (i<p)){
      U.pls<-rowswap(U.pls,i,i+1)
    }
    U.pls<-rowscale(U.pls,i,1/U.pls[i,i])
    if (i<p){
      for (k in (i+1):p){
        a<--U.pls[k,i]/U.pls[i,i]
        U.pls<-rowtransform(U.pls,i,k,a)
      }
    }
  }
  return(U.pls)
}
rowreduc1(U.pls)

# function for backsubstitution
backsub<-function(U.pls){
  for (i in (p-1):1){
    for (j in i:1){
      k<--U.pls[j,i+1]
      U.pls<-rowtransform(U.pls,i+1,j,k)
    }
  }
  return(matrix(U.pls[,p+1], nrow=p))
}
backsub(rowreduc1(U.pls))

# Find inverse matrix
invmat<-function(A){
  p<-nrow(A)
  E<-diag(rep(1,p))
  A.E<-cbind(A, E)
  invA<-A
  for (i in 1:p){
    temp<-rowreduc1(cbind(A.E[,1:p], E[,i]))
    temp<-backsub(temp)
    invA[,i]<-t(temp)
  }
  return(invA)
}
invmat(A)
solve(A)

A <- matrix(c(0,-5,4,0,-2,1,1,-4,6),byrow=T,nrow=3); A
pivoting<-function(m, row.c){
  if (m[row.c,row.c]==0){
    temp<-which(m[,row.c]!=0)
    row.n<-min(temp[temp>row.c])
    U.pls<-rowswap(m, row.c, row.n)
  } else {
    U.pls<-m
  }
  return(U.pls)
}
pivoting(A,1)

# row elementary reduction Using 'pivoting'
U.pls<-cbind(A,b)
U.pls
rowreduc2<-function(U.pls){
  nrows<-nrow(U.pls)
  for (row.c in 1:(nrows-1)){
    if (U.pls[row.c:row.c]==0){
      U.pls<-pivoting(U.pls, row.c)
    }
    U.pls<-rowreduc1(U.pls)
  }
  return(U.pls)
}
rowreduc2(U.pls)

# find inverse matrix Using pivoting
invmat2<-function(A){
  p<-nrow(A)
  E<-diag(rep(1,p))
  A.E<-cbind(A, E)
  invA<-A
  for (i in 1:p){
    temp<-rowreduc2(cbind(A.E[,1:p], E[,i]))
    temp<-backsub(temp)
    invA[,i]<-t(temp)
  }
  return(invA)
}
A
solve(A)
invmat2(A)

# find eigen value, eigen vector : power method 1
A <- matrix(c(1,0.4,0.2,0.4,1,0.4,0.2,0.4,1),byrow=T,nrow=3)
x0<-as.vector(c(1,0,0)); diff<-0.5; eps<-0.0001

vnorm<-function(x) sqrt(sum(x*x))
while(diff>eps){
  x1<-A%*%x0
  lambda1<-vnorm(x1)/vnorm(x0)
  x1<-x1/vnorm(x1)
  diff<-vnorm(x1-x0)
  x0<-x1
}

# keep going to find next eigen value and vector
x0<-as.vector(c(1,0,0)); diff<-0.5; eps<-0.0001
A1<-A-lambda1*x0%*%t(x0)

while(diff>eps){
  x1<-A1%*%x0
  lambda2<-vnorm(x1)/vnorm(x0)
  x1<-x1/vnorm(x1)
  diff<-vnorm(x1-x0)
  x0<-x1
}

lambda2
x0

# Another version of power method
powermethod<-function(A, x0, eps=1e-6, maxiter=100){
  xold<-x0
  steps<-1
  repeat{
    znew<-A%*%xold
    xnew<-znew/vnorm(znew)
    lamnew<-t(xnew)%*%A%*%xnew
    if (vnorm(abs(xnew)-abs(xold))<eps) break
    xold<-xnew
    steps<-steps+1
    if (steps==maxiter) break
  }
  lambda<-t(xnew)%*%A%*%xnew
  list(lambda, xnew, steps)
}
powermethod(A, c(1,0,0))

# solving difference equation : Using eigen values / vectors
solvediff<-function(A, k, x0,x1){
  eigenval<-eigen(A)$values
  diagmat<-diag(c(eigenval[1]^k, eigenval[2]^k), 2)
  p<-eigen(A)$vectors
  xvec<-matrix(c(x1,x0), 2)
  xk<-p%*%diagmat%*%solve(p)%*%xvec
  return(xk[2])
}

cmat <- matrix(c(1,1, 1,0),2,2)
solvediff(cmat, 10,1,1)

# power to matrix : recursive programing
powermat<-function(mat, k){
  if (k==0) return(diag(rep(1,nrow(mat)), nrow=nrow(mat)))
  if (k==1) return(mat)
  if (k>1) return(mat%*%powermat(mat, k-1))
}
mat<-matrix(1:9, nrow=3)
powermat(mat,2)

powermat(cmat, 10)%*%matrix(c(1,1), nrow=2)

# Stochastic process
# random walk
win<-sample(c(-1,1), 50, replace=T)
cum.sum<-cumsum(win)

# sample path
par(mfrow=c(2,2))
for (i in 1:4){
  win<-sample(c(-1,1), 50, replace=T)
  cum.sum<-cumsum(win)
  plot(cum.sum, type="l")
}

# Monte carlo simulation : random walk
B.win<-function(n){
  win<-sample(c(-1,1), n, replace = T)
  sum(win)
}
B.win(50)
BV.win<-replicate(1000, B.win(50))
table(BV.win)
par(mfrow=c(1,1))
plot(table(BV.win)/1000)
mean(BV.win==0)

# Markov chain
pmat<-matrix(c(1/3,2/3,1/3,2/1), 2, byrow=T)
pmat%*%rep(1,2)

# Using Monte-Carlo simulation 
pmat<-matrix(c(1/3,2/3,1/3,2/1), 2, byrow=T)
Markov.sim<-function(n, pmat, x0){
  sim<-numeric(n)
  m<-ncol(pmat)
  sim[1]<-x0
  for (i in 2:n){
    nextstate<-sample(1:m, 1, prob=pmat[sim[i-1],])
    sim[i]<-nextstate
  }
  return(sim)
}
bookcafe<-Markov.sim(12, pmat, 1)
bookcafe

# Markov chain for wheather forecast

Wmat <- matrix(c(1/2,1/4,1/4,1/2, 0,1/2,1/4,1/4,1/2),nrow=3,ncol=3,byrow=TRUE)
Wmat[1,]%*%Wmat[,3]

powermat <- function(mat,k) {
  if (k == 0) return (diag(dim(mat)[1])) 
  if (k == 1) return(mat)
  if (k > 1) return( mat %*% powermat(mat, k-1))
}

pow<-4; powerW<-powermat(Wmat,pow)
powerW
sum(c(0.5,0.5,0)*powerW[,3])
c(0.5,0.5,0)%*%powerW
sum(c(0.5,0.5,0)*powerW[,3])
c(0.5,0.5,0)%*%powerW

# Simulation for markov chain
M<-ncol(Wmat); n<-1e+5; x<-rep(0,n); pow<-4
for (j in 1:n){
  temp<-sample(M, 1, p=c(0.5,0.5,0))
  for (i in 1:pow){
    temp<-sample(M, 1, p=Wmat[temp,])
  }
  x[j]<-temp
}
table(x)/length(x)

Pmat <- matrix(c(1/3,2/3,2/3,1/3),nrow=2)

markovsim<-function(n, Pmat,x0){
  sim<-numeric(n)
  sim[1]<-x0
  m<-ncol(Pmat)
  for (i in 2:n){
    temp<-sample(m, 1, p=Pmat[sim[i-1],])
    sim[i]<-temp
  }
  return(sim)
}
markovsim(12,Pmat,1)

# Gambler's ruin problem : Using Markov chain
p<-0.6; q<-1-p; n<-8
Tmat<-matrix(0, n+1, n+1)
for (i in 2:n){
  Tmat[i,i-1] <-q
  Tmat[i,i+1] <-p
}
Tmat[1,1]<-1; Tmat[n+1, n+1]<-1
Tmat
rownames(Tmat)<-0:n; colnames(Tmat)<-0:n
Tmat
sum(0:8%*%powermat(Tmat,4)[4,])

# Gambler's ruin problem : Using simulation
Markov.sim2<-function(Pmat, n,power ,x0){
  M<-nrow(Pmat)
  k<-numeric(n)
  for (j in 1:n){
    temp<-x0
    for (i in 1:pow){
      temp<-sample(M,1,p=Pmat[temp,])
    }
    k[j]<-temp
  }
  return(mean(k))
}
Markov.sim2(Tmat, 1e+5, 4,3)

# Markov chain : Stationary distribution
Pmat <- matrix(c(0.1,0.2,0.4,0.3,0.4,0,0.4,
                 0.2,0.3,0.3,0,0.4,0.2,0.1,0.4,0.3),
               nrow=4, byrow=TRUE)
label <- c("Fitness","Rock-Climbing","Squash","Yoga")
rownames(Pmat) <- label; colnames(Pmat) <- label
init<-rep(1/4,4)
P100<-powermat(Pmat, 100)
init%*%P100
P100
states <- c("F","R","S","Y")

Markovsim2<-function(n, Pmat, states, init, x0){
  sim<-numeric(n+1); m<-1:length(init)
  if (missing(x0)){
    sim[1]<-sample(m,1,p=init)
  }
  for (i in 2:(n+1)){
    nextstate<-sample(m,1,p=Pmat[sim[i-1],])
    sim[i]<-nextstate
  }
  return(states[sim])
}
Markovsim2(100,Pmat,states,init)

r<-replicate(10000,Markovsim2(100,Pmat,states,init)[101])
table(r)/10000

# Markov chain : graduation rate - limiting distribution
init <- c(0,1,0,0,0,0)
Grmat <- matrix(c(1,0,0,0,0,0,0.06,0.03,0.91,0,0,0,0.06,0,0.03,0.91,0,0,0.04,
                  0,0,0.03,0.93,0,0.04,0,0,0,0.03,0.93,0,0,0,0,0,1),nrow=6,byrow=T)
states <- c("gu","1","2","3","4","gr")
rownames(Grmat) <- states
colnames(Grmat) <- states
Grmat

init%*%powermat(Grmat, 100)
Markovsim2(100, Grmat, states, init)
r1<-replicate(10000,Markovsim2(100, Grmat, states, init)[6] )
table(r1)/10000

# Markov chain : gambler's ruin problem - limiting distribution
p<-0.6; q<-1-p; n<-8
Tmat<-matrix(0, n+1, n+1)
for (i in 2:n){
  Tmat[i,i-1] <-q
  Tmat[i,i+1] <-p
}
Tmat[1,1]<-1; Tmat[n+1, n+1]<-1
Tmat
rownames(Tmat)<-0:n; colnames(Tmat)<-0:n
Tmat
powermat(Tmat, 100)
init <- c(0,0,0,1,0,0,0,0,0)
states <- 0:n

# how about simulations?
Markovsim2(100, Tmat, states, init)[101]
r2<-replicate(10000, Markovsim2(100,Tmat, states,init)[101])
table(r2)/10000

# Stationary distribution
Wmat
stationary1<-function(Wmat){
  rown<-length(Wmat[,1])
  coln<-length(Wmat[1,])
  Rmat<-matrix(1, nrow=rown, ncol=coln)
  zerovec<-matrix(0,nrow=1, ncol=coln )
  Rmat[,1:(coln-1)]<-Wmat[,1:(coln-1)]-diag(rep(1, coln))[,1:(coln-1)]
  bvec<-matrix(1, nrow=1, ncol=coln)
  bvec[,1:(coln-1)]<-zerovec[,1:(coln-1)]
  svec<-bvec%*%solve(Rmat)
  return(svec)
}
stationary1(Wmat)
powermat(Wmat, 100)

# Stationary distribution Practice
Wmat
station<-function(Wmat){
  coln<-length(Wmat[1,]); rown<-length(Wmat[,1])
  Rmat<-matrix(1,nrow=rown, ncol=coln)
  Rmat[,1:(coln-1)]<-(Wmat-diag(1, nrow=rown))[,1:(coln-1)]
  zerovec<-matrix(0, nrow=1, ncol=coln)
  onevec<-matrix(1, nrow=1, ncol=coln)
  onevec[,1:(coln-1)]<-zerovec[,1:(coln-1)]
  st<-onevec%*%solve(Rmat)
  return(st)
}
station(Wmat)
powermat(Wmat, 100)
station()
# Numerical differentiation 
fx<-function(x) sin(x)
x0<-0.5; h <- 1e-5
(fx(x0+h)-fx(x0))/h
(fx(x0+h)-fx(x0-h))/(2*h)
cos(x0)

# Richardson Extrapolation
f<-function(x) sin(x)
h<-1e-5
f1<-function(x, h){
  (f(x+h)-f(x-h))/(2*h)
}
d1<-f1(0.5, h)
d2<-f1(0.5, 2*h)
print((4*d1-d2)/3, 16)
print(cos(0.5), 16)

# Secant method
secant1<-function(g, x0, x1, tol=1e-9, max.iter=100){
  f0<-g(x0); f1<-g(x1); iter<-0
  while (abs(f1)>tol && iter < max.iter){
    if (f1==f0) return("Algorithm failed with f1==f0")
  x2<-x1-f(x1)*(x1-x0)/(f1-f0)
  x0<-x1; x1<-x2; f0<-f1
  f1<-g(x1); iter<-iter+1
  }
  return(x0)
}
g<-function(x) log(x)-exp(-x)
secant1(g, 1,2)

# Numerical integration
# mid point rule
f <- function(x) 1/sqrt(2*pi)*exp(-x^2/2)
midpoint<-function(f, a, b, n){
  h<-(b-a)/n
  xgrid<-seq(a,b-h, by=h)
  fval<-f(xgrid+h/2)
  I_M<-h*sum(fval)
  return(I_M)
}

midpoint(f,0,1,20)
pnorm(1,0,1)-0.5

# Trapezoidal rule
trapezoid<-function(f, a, b, n){
  h<-(b-a)/n
  xgrid<-seq(a, b, by=h)
  fval<-f(xgrid)
  I_T<-h*(fval[1]+2*sum(fval[2:n])+fval[n+1])/2
  return(I_T)
}
trapezoid(f, 0,1,20)

#simpson rule
simpson<-function(a,b,n){
  I_S<-(trapezoid(f, a, b,n)+2*midpoint(f, a, b, n))/3
  return(I_S)
}
simpson(0,1,100)
pnorm(1,0,1)-0.5

# Using built in function : Integrate
f<-function(x) 1/sqrt(2*pi)*exp(-x^2/2)
integrate(f, 0, 1)
f1<- function(x) min(0, x) 
integrate(f1, -1, 1)
# integrate function is only used with functions can give vector output
f1(c(0,1,2))
f2<-Vectorize(f1)
integrate(f2, -1, 1)
f3<-function(x) pmin(0, x)
integrate(f3, -1, 1)          

# Monte-carlo integration : Treat integral as Expectation
x<-runif(1e+5)
mean(exp(-x))

x<-runif(1e+5, 0, 2*pi)
f<-function(x) exp(cos(x))
mean(2*pi*f(x))
midpoint(f, 0, 2*pi, 1e+4)

# Random number generation
n<-10; N<-1000; M<-rep(0, N)
for (i in 1:N){
  M[i]<-max(runif(n))
}
M
hist(M, prob=T)
x<-seq(0, 1, 0.001)
y<-dbeta(x, n, 1)
lines(y~x, col=2)

# cosine theta
N<-1000
theta<-runif(N)*2*pi
cos<-cos(theta)
hist(cos, prob=T)

x <- seq(-1,1,0.01)
y <- 1/(pi*sqrt(1-x^2))
lines(x,y)

# probability integral transformation
# Exp(1)
U<-runif(1e+5)
f<-function(x) -log(1-x)
X<-f(U)
par(mfrow=c(1,2))
hist(X, prob=T, breaks = "Sturges")
e<-rexp(1e+5, 1)
hist(e, prob=T, breaks = "Sturges")
# random number generating beta distribution
n<-1e+4
U<-runif(n)
alpha<-4
X<-U^(1/alpha)
Y<-rbeta(n, alpha, 1)
par(mfrow=c(1,2))
hist(X, prob=T); hist(Y, prob=Y)
ks.test(X,Y)

bin.sim<-function(n, p){
  x<-0; U<-runif(1); px<-(1-p)^n; Fx<-px
  while(Fx<U){
    x<-x+1
    px<-px*(n-x+1)*p/(x*(1-p))
    Fx<-Fx+px
  }
  return(x)
}
bin.sim(10, 0.4)
se<-replicate(1e+3, bin.sim(10, .4))
y<-rbinom(1e+3, 10, .4)

# bernoulli simulation
x<-0; p<-.6; n<-1
for (i in 1:n){
  U<-runif(1)
  if (U>p) x<-x+1
}

# Rayleigh random number generating
U<-runif(1e+4)
x<--log(U)
sqrt(x)

# Gamma, Laplace random number generating 
n<-1e+4
u1<-runif(n); u2<-runif(n)
ed1<--log(u1); ed2<--log(u2)
gam<-ed1+ed2
lei<-ed1-ed2

# geometric random number generating : denote # of trials
x<-1; p<-0.4; Suc<-FALSE
while (!Suc){
  U<-runif(1)
  if (U<p){
    Suc<-TRUE
  } else {
    x<-x+1
  }
}
x

# generating sequence of bernoulli trials : geometric random number
n<-20; which.max((runif(n)<p)==TRUE)
Btrials<-rbinom(1000,1,p) 
x<-which(Btrials==1)[1]
x

# geometric random number generating : denote # of failure
geo.sim<-function(p){
  x<-0; px<-p; Fx<-p
  U<-runif(1)
  while(Fx<U){
    x<-x+1
    px<-px*(1-p)
    Fx<-Fx+px
  }
  return(x)
}
geo.sim(0.4)
y<-replicate(1e+4, geo.sim(0.4))
mean(y)
y1<-rgeom(1e+4, 0.4)

# Negative binomial random number generating : sum of geometric distribution
r<-replicate(10, geo.sim(0.4))
sum(r)

# coupon collector's problem : probability at fixed n
cards <- 1:12
sam.cards<-sample(cards, 20, replace=T)
length(unique(sam.cards))
table(sam.cards)

collector <- function(n,m){
  x<-sample(1:n, m, replace = T)
  ifelse(length(unique(x))==n, "yes", "no")
}
collector(12,200)
table(replicate(100, collector(12, 20)))/100

# coupon collector's problem : Expectation
couponcollector<-function(n){
  my.cards<-sample(1:n, 1, replace = T); count<-1
  u.cards<-length(unique(my.cards))
  while(u.cards<n){
    sam.cards<-sample(1:n, 1, replace = T)
    my.cards<-c(my.cards, sam.cards)
    u.cards<-length(unique(my.cards))
    count<-count+1
  }
  return(count)
}
couponcollector(12)
r<-replicate(1e+4, couponcollector(12))
mean(r)
12*sum(1/1:12) # exact expectation value of coupon collector's problem

# counting process
# generating poisson random number
lambda<-2; N<-1e+4; z<-rep(0,N)
for (i in 1:N){
  count<-0; time<-0
  while(time < 1){
    time<-time + rexp(1, lambda)
    count<-count+1
  }
  z[i]<-count-1
}

# Poisson random number : Using relationship between poisson process and exponential random number
genpoi<-function(lambda, at){
  x<-rexp(100, lambda)
  if (sum(x)<at){
    return(NA)
  } 
  if (x[1]>at){
    return(0)
  } else {
    return(max(which(cumsum(x)<at)))
  }
}
r<-replicate(1e+4, genpoi(2,1))

# Waiting time
mytime<-40; lambda<-1/20; waitingtime<-vector(length=1000)
for (i in 1:1000){
  arrival<-rexp(1, lambda)
  wait<-arrival-mytime
  while(wait<0){
    arrival<-arrival+rexp(1, lambda)
    wait<-arrival-mytime
  }
  waitingtime[i]<-wait
}
mean(waitingtime)

# memoryless property
trials<-1e+4; pA<-numeric(trials); pB<-numeric(trials)
for (i in 1:trials){
  bus<-rexp(1, 1/20)
  pA[i]<-bus
  while(bus<10){bus<-bus+rexp(1, 1/20)}
  pB[i]<-bus-10
}
mean(pA); mean(pB)

# Box - Muller transformation
M<-1e+4; x<-numeric(M); y<-numeric(M)
for (i in 1:M){
  u<-runif(2)
  x[i]<-sqrt(-2*log(u[1]))*cos(2*pi*u[2])
  y[i]<-sqrt(-2*log(u[1]))*sin(2*pi*u[2])
}
x; y

# Central Limit Theorem : Normal random number generating
nsim<-1e+4; n<-30 
x<-matrix(runif(nsim*n), nsim, n)
zn<-sqrt(12*n)*(rowMeans(x)-0.5)
hist(zn, prob=T)

# Slutsky's Theorem : normal random number generating - for large n
nsim<-1e+4; n<-30
x<-matrix(runif(nsim*n), nsim, n)
sx<-apply(x,1, sd)
tn<-sqrt(n)*(rowMeans(x)-0.5)/sx
hist(tn, prob=T)

# T(n-1) random number generating
nsim<-1e+4; n<-4; x<-matrix(rnorm(nsim*n, 1, 2), nsim, n)
sx<-apply(x, 1, sd)
t3<-sqrt(n)*(rowMeans(x)-1)/sx
hist(t3, prob=T, nclass=50,xlim=c(-10,10),ylim=c(0,0.5))

par(mfrow=c(1,2))

# T(n) using definition
tgen<-function(n){
  z<-rnorm(1,0,1)
  x<-rnorm(n,0,1)
  chi<-sum(x^2)
  tn<-z/sqrt(chi/n)
  return(tn)
}
tn<-replicate(1e+4, tgen(1000))
hist(tn, prob=T)

# T(n) using composition
tgen2<-function(n){
  v<-rgamma(1, n/2,1/2)
  tn<-rnorm(1,0,sqrt(n/v))
  return(tn)
}
tn<-replicate(1e+4, tgen2(3))
hist(tn, prob=T)
x<-seq(-3,3,0.01); lines(x,dnorm(x,0,1),col="red")
x1<-seq(-10,10,0.01); lines(x1,dt(x1,3),col="blue",lty=2,lwd=2)

# composition : normal mixture
mixnorm<-function(mu1, s1, mu2, s2){
  u<-runif(1)
  if (u<0.5){
    x<-rnorm(1, mu1, sqrt(s1))
  } else {
    x<-rnorm(1, mu2, sqrt(s2))
  }
  return(x)
}
xmix<-replicate(1e+4, mixnorm(0,1,3,2))
hist(xmix,prob=T,nclass=30,xlim=c(-5,10),ylim=c(0,0.5))
x<-seq(-5,10,0.01);
lines(x,0.5*dnorm(x,0,1)+0.5*dnorm(x,3,sqrt(2)),col="red")

# Beta - binomial distributionm : Bayesian approach
betabin<-function(n){
  p<-runif(1)
  xbin<-rbinom(1, n, p)
}
xbetabin<-replicate(1e+4, betabin(5))
table(xbetabin)/1e+4

# Binomial - poisson mixture : poisson(lambda*p)
gen.exp<-function(lambda){
  u<-runif(1)
  x<--log(u)/lambda
  return(x)
}

gen.poi<-function(lambda){
  count<-0
  time<-0
  while(time<1){
    time<-time+gen.exp(lambda)
    count<-count+1
  }
  return(count-1)
}
r<-replicate(1e+4, gen.poi(5))

gen.bin<-function(n,p){
  U<-runif(1); px<-(1-p)^n; Fx<-px; x<-0
  while(Fx<U){
    x<-x+1
    px<-px*(n-x+1)*p/(x*(1-p))
    Fx<-Fx+px
  }
  return(x)
}

call<-numeric(1e+4)
for (i in 1:1e+4){
  call[i]<-gen.bin(r[i], 0.4)
}
mean(call)

# Accept - reject algorithm : triangle distribution
xstar<-runif(10000, 0, 2)
U<-runif(10000)
x<-xstar[U<1-abs(1-xstar)]
hist(x, prob=T)

# User defined function for triagle distribution
rej1<-function(fx,a,b,M){
  while(TRUE){
    x<-runif(1, a, b); u<-runif(1,0,M)
    if (u<fx(x)) return(x)
  }
}
fx<-function(x){
  if (x>0 && x<2){
    return(1-abs(1-x))
  } else {
    return(0)
  }
}

rejsam<-numeric(1e+4)
for (i in 1:1e+4){
  rejsam[i]<-rej1(fx, 0, 2, 1)
}
hist(rejsam, prob=T)

# Accept - reject algorithm for random number from beta(2.7, 6.3) / candidata : Unif(0, M)
Nsim<-2500; a<-2.7; b<-6.3; M<-2.67
u<-runif(Nsim, 0, M); xstar<-runif(Nsim)
x<-xstar[u<dbeta(xstar, a, b)]
plot(xstar,u,xlab="xstar",ylab="U from Uniform(0,M)",xlim=c(0,1),ylim=c(0,2.7))
u1=u[u<dbeta(xstar,a,b)]
points(x,u1,pch=19,col=2)
z=seq(0,1,0.01)
lines(z,dbeta(z,a,b),lwd=2,col=3)

# Accept - reject algorithm for random number from beta(2.7, 6.3) / candidata : beta(2,6)
Nsim<-2500; a<-2.7; b<-6.3; M<-1.67

gen.beta<-function(a,b){
  r1<-replicate(a,gen.exp(3))
  r2<-replicate(b,gen.exp(3))
  r_1<-sum(r1); r_2<-sum(r2)
  x<-r_1/(r_1+r_2)
  return(x)
}

xstar<-replicate(Nsim,gen.beta(2,6))
u<-runif(Nsim)
x<-xstar[u<dbeta(xstar, a,b)/(M*dbeta(xstar, 2,6))]
plot(xstar, M*u*dbeta(xstar, 2, 6))
u1<-u[u<dbeta(xstar, a,b)/(M*dbeta(xstar, 2,6))]
points(x,u1*M*dbeta(x,2,6),pch=19,col=2)
z=seq(0,1,0.01)
lines(z,M*dbeta(z,2,6),lwd=2,col=5)
lines(z,dbeta(z,a,b),lwd=2,col=3)

# Truncated normal distribution : using candidate - Shiftied exponential distribution
trunc.norm<-function(a, lambda){
  while(TRUE){
    U<-runif(1); U1<-runif(1)
    xstar<-a-log(U)/lambda
    f<-function(x, a){
      exp(-(x-a)^2/2)
    }
    if (U1<f(xstar, a)){
      return(xstar)
    }
  }
}

r<-replicate(1e+4, trunc.norm(1,1))
hist(r, prob=T,ylim=c(0,2))

f<-function(x){1/sqrt(2*pi)*exp(-x^2/2)/(1-pnorm(1,0,1))}
x<-seq(1, 5, 0.001)
lines(f(x)~x, col="red")

# box - muller transformation + accept - reject algorithm
norm.gen<-function(M){
  n<-0; x<-NULL
  repeat{
    u1<-2*runif(1)-1; u2<-2*runif(1)-1
    R2<-u1^2+u2^2
    if (R2<=1){
      x<-c(x, sqrt(-2*log(R2))*u1/sqrt(R2))
      n<-n+1
    }
    if (n==M) break
  }
  return(x)
}

num<-norm.gen(1e+4)
hist(num, prob=T)

# Monte-carlo integration 
# 1. naive transform
u<-runif(1e+4,0, log(10))
f<-function(x){
  x^2*exp(-x)
}

log(10)*mean(f(u))

# 2. parameter transformation
f<-function(x) (log(x))^2
u<-runif(1e+5, 0.1, 1)
.9*mean(f(u))

# 3. is it a pdf?
exp.gen<-function(lambda, n){
  -log(runif(n))/lambda
}

gam.gen<-function(lambda,n){
  x<-sum(exp.gen(lambda,n))
  return(x)
}

r<-replicate(1e+5, gam.gen(1, 3))
2*mean(r<log(10))

# Approximate 'pi' by using monte - carlo simulation
app.pi<-function(u1, u2){
  R2<-u1^2+u2^2
  4*sum(R2<1)/length(u1)
}

u1<-runif(1e+5)
u2<-runif(1e+5)
app.pi(u1, u2)

# Approximate 'pi' : Buffon's needle
buffon<-function(L){
  d<-runif(1,0,L/2)
  theta<-runif(1, 0, pi/2)
  x<-L*cos(theta)/2
  if (d<x){
    return(1)
  } else {
    return(0)
  }
}

r<-replicate(1e+6, buffon(1))
2/mean(r)

# Hit & Miss method - most naive monte-carlo method
hit.miss<-function(fx, a,b,c,d,n){
  z<-numeric(n)
  x<-runif(n, a, b)
  y<-runif(n, c, d)
  for (i in 1:n){
    if (y[i]<=f(x[i])){
      z[i]<-1
    } else {
      z[i]<-0
    }
  }
  return(mean(z)*(b-a)*(d-c)+c*(b-a))
}

f<-function(x) x^3-7*x^2+1

hit.miss(f, 0, 1, -6, 2, 1e+5)
integrate(f, 0, 1)

# Weak law of large number
U<-runif(1e+5)
mean(f(U))

# Monte carlo integration
exp.gen<-function(lambda,n){
  U<-runif(n)
  x<--log(U)/lambda
  return(x)
}

r<-exp.gen(2, 1e+5)
mean(r)/2

norm.gen<-function(n){
  theta<-runif(n)*2*pi
  t<-exp.gen(1/2, n)
  z<-sqrt(t)*cos(theta)
  return(z)
}

r<-norm.gen(1e+5)
mean((sin(r))^2)*sqrt(2*pi)

# Using uniform rv 1

g<-function(x) 1/sqrt(2*pi)*exp(-x^2/2)
U<-runif(1e+5)*1.96
0.5-1.96*mean(g(U))

# Using uniform rv 2
Monte.den.norm<-function(x,n){
  U<-runif(n)
  val<-x*exp(-x^2*U^2/2)/sqrt(2*pi)
  return(mean(val))
}

0.5-Monte.den.norm(1.96, 1e+5)
r<-norm.gen(1e+5)

# Using indicator function
r<-norm.gen(1e+5)
ind<-(r>1.96)
mean(ind)
var(ind)/1e+5

# Estimate of Monte carlo and its variance
U<-runif(1e+5)
val<-1-U^2
mean(val)

ara<-function(n){
  while(TRUE){
    U1<-runif(n)
    U2<-2*runif(n)
    f<-function(x) 2*(1-x)
    if (U2<f(U1)){
      return(U1)
    }
  }
}
r<-replicate(1e+5,ara(1))
mean(r)/2+0.5

# Importance sampling
U<-runif(1e+5)
x<-2/U^(1/4)
val<-(2+x)^1.5/64
mean(val)
var(val)/1e+5

U<-runif(1e+5,1,30)
val<-log(1+U)-log(1-2*U+U^2)
29*mean(val)

f<-function(x) log(1+x)-log(1-2*x+x^2)
integrate(f, 1, 30)

U1<-runif(7000, 1, 3)
U2<-runif(93000, 3, 30)
2*mean(f(U1))+27*mean(f(U2))

0.5-Monte.den.norm(4.5,1e+5)

# Importance sampling for normal property
U<-runif(1e+5)
x<--log(U)+4.5

g<-function(x) exp(-x^2/2+x-4.5)/sqrt(2*pi)
mean(g(x))
pnorm(-4.5)

# naive method of monte - carlo
U<-runif(1e+5, 0, 1)
val<-sqrt(1-U^2)
mean(val)


ara2<-function(n){
  f<-function(x) 6*(1-0.5*x^2)/5
  while(TRUE){
  U1<-runif(n)
  U2<-6*runif(n)/5
  if (U2<f(U1)){
    return(U1)
  }
  }
}

r<-replicate(1e+5, ara2(1))
r
val<-sqrt(1-r^2)/(1.2*(1-0.5*r^2))
mean(val)

f<-function(x) sqrt(1-x^2)
integrate(f, 0, 1)

U<-runif(1e+5)
val<-1/sqrt(U)
mean(val)
var(val)/1e+5

f<-function(x) 1/sqrt(x)

integrate(f, 0, 1)

r<-norm.gen(1e+5)
ind<-(r>0 & r<1)
sqrt(2*pi)*mean(ind)

truncated.cauchy<-function(n){
  U<-runif(n)
  x<-sqrt(2)*tan(U*atan(1/sqrt(2)))
  return(x)
}

r<-truncated.cauchy(1e+5)

g<-function(x) exp(-x^2/2)*(1+x^2/2)*sqrt(2)*atan(1/sqrt(2))
val<-g(r)
mean(val)

# Choice of importance function
# 1 : uniform
U<-runif(1e+5)
f<-function(x) exp(-x)/(1+x^2)
val<-f(U)
var(val)
mean(val)

# 2 : exponential distribution
r<-exp.gen(1, 1e+5)
val1<-1/(1+r^2)
var(val1)
mean(val1)

# 3 : cauchy(0,1)
cauchy.gen<-function(n){
  u<-runif(n)
  x<-tan(pi*(u-0.5))
  return(x)
}
r<-cauchy.gen(1e+5)

val2<-pi*exp(-r)
mean(val2)

# 4 : truncated exponential
truncated.exp<-function(n){
  u<-runif(n)
  x<--log(1-u*(1-exp(-1)))
}

r<-truncated.exp(1e+5)
val3<-(1-exp(-1))/(1+r^2)
var(val3)
mean(val3)

# 5 : truncated cauchy
truncated.cauch2<-function(n){
  u<-runif(n)
  x<-tan(pi*u/4)
  return(x)
}

r<-truncated.cauch2(1e+5)
val4<-pi*exp(-r)/4
var(val4)
mean(val4)
var(val); var(val1); var(val2); var(val3); var(val4)

# MCMC : Metropolis - Hastings algorithm
# proposal : uniform
n<-1e+3
x<-numeric(n)
for (i in 2:n){
  y<-runif(1,x[i-1]-1,x[i-1]+1)
  alpha<-exp(-(y^2-x[i-1]^2)/2)
  if (runif(1)<alpha){
    x[i]<-y
  } else {
    x[i]<-x[i-1]
  }
}
hist(x, prob=T)
ts.plot(x, col=3)

# proposal : normal
metronorm<-function(n,b,init){
  x<-rep(init, n)
  f<-function(x) 1/sqrt(2*pi)*exp(-x^2/2)
  for (i in 2:n){
    y<-b*norm.gen(1)+x[i-1]
    alpha<-f(y)/f(x[i-1])
    if (runif(1)<alpha){
      x[i]<-y
    } else {
      x[i]<-x[i-1]
    }
  }
  return(x)
}
r<-metronorm(2000, 1, -10)
ts.plot(r, col=3)

r1<-metronorm(2000, 0.5, -10)
ts.plot(r1, col=3)

r2<-metronorm(2000, 10, -10)
ts.plot(r2)
acf(r2)
acf(r)
acf(r1)

# zipf distribution
# Arbitrary proposal Q matrix
m=10
state=seq(1,m)
qmat=matrix(0,m,m)
for (i in 1:m)
{
  for (j in 1:m)
  {
    qmat[i,j]=1/2*(j==i-1)+1/2*(j==i+1)
  }
}
qmat[1,1]=qmat[m,m]=1/2    
qmat
dim(qmat)[1]

metrozipf<-function(N, qmat, init,a){
  x<-rep(init,N)
  for (i in 2:N){
    y<-sample(1:dim(qmat)[1], 1, prob=qmat[x[i-1],])
    alpha<-min(x[i-1]^a/y^a, 1)
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

zipf<-metrozipf(1e+6, qmat, 2, 2)
zipf_new<-burn_thin(200, 80, 1000, zipf)
hist(zipf_new, prob=T)
ts.plot(zipf_new)
acf(zipf_new)

# MH algorithm 
metro1<-function(n, b, init){
  x<-rep(init, n)
  f<-function(x) exp(-x^4+3*log(1+abs(x)))
  for (i in 2:n){
    y<-sqrt(b)*norm.gen(1)+x[i-1]
    alpha<-min(f(y)/f(x[i-1]), 1)
    if (runif(1) < alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

l2<-metro1(1e+5, 1, 3)
l2_new<-burn_thin(200, 50, 1000, l2)
hist(l2_new, prob=T)
acf(l2_new)
ts.plot(l2_new)

# mixture of power & sine
metro2<-function(n,b,init){
  x<-rep(init, n)
  f<-function(x) (sin(x)/x)^2
  for (i in 2:n){
    y<-sqrt(b)*norm.gen(1)+x[i-1]
    if (abs(y)<=3*pi){
      alpha<-min(f(y)/f(x[i-1]),1)
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

l3<-metro2(1e+5, 1, -10)
hist(l3)
l3_new<-burn_thin(250, 55, 1000, l3)
ts.plot(l3_new)

# Independence sampler : beta(2.7, 6.3)
beta.sampler<-function(n,init, a,b){
  x<-rep(init, n)
  f<-function(x) x^(a-1)*(1-x)^(b-1)
  for (i in 2:n){
    y<-runif(1)
    alpha<-min(1, f(y)/f(x[i-1]))
    if (runif(1)<alpha){
      x[i]<-y
    }else{
      x[i]<-x[i-1]
    }
  }
  return(x)
}

l4<-beta.sampler(1e+5, 0.5, 2.7, 6.3)
hist(l4)
acf(l4)
ts.plot(l4)
l4_new<-burn_thin(200, 50, 1000, l4)
hist(l4_new, prob=T)
x<-seq(0.01,0.99, 0.001)
f<-function(x) x^1.7*(1-x)^5.3/beta(2.7, 6.3)
lines(f(x)~x)

# Gibbs sampler
gibbs_norm<-function(cov, init, n){
  x<-matrix(rep(init, 2*n), nrow=n, ncol=2)
  for (i in 2:n){
    x1<-norm.gen(1)*sqrt(1-cov^2)+cov*x[i-1,2]
    x[i,1]<-x1
    x[i,2]<-norm.gen(1)*sqrt(1-cov^2)+cov*x[i-1,1]
  }
  return(x)
}
gibbs<-gibbs_norm(0.6,3,1e+5)
hist(gibbs[,1], prob=T)
hist(gibbs[,2], prob=T, add=T)

# Gibbs sampler 2
invx<-function(y, b, u){
  x<--log(1-u*(1-exp(-b*y)))/y
  return(x)
}
invy<-function(x, b, u){
  y<--log(1-u*(1-exp(-b*x)))/x
  return(y)
}

gibbs2<-function(n, init,b){
  x<-matrix(rep(init,n), nrow=n, ncol=2)
  for (i in 2:n){
    x[i,1]<-invx(x[i-1,2],b,runif(1))
    x[i,2]<-invy(x[i,1],b,runif(1))
  }
  return(x)
}

gib<-gibbs2(1e+4, runif(1, 0, 5), 5)
gib
hist(x[,1],nclass=30, add=T)
