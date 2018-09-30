###CH 2. one sample location parameter problems###

install.packages("BSDA")
library(BSDA)

# Sign test
x<-c(1.5,2.2,0.9, 1.3, 2, 1.6,1.5,2,1.2,1.7)

# md : specified value at null + additional option 'conf.level' : provide confidence interval
SIGN.test(x, md=1.8, alternative = "two.sided", conf.level = 0.95)

# sorting data
sort(x)

# Wilcoxon signed rank test : robust to outliers
x<-c(188, 211.2, 170.8, 212.4, 156.9, 223.1, 235.9, 183.9, 214.4, 221, 162,222.8, 174.1, 210.3, 195.2)
# Wilcoxon signed rank sum test could be used when testing null with parameter "mean"
# "conf.int=T" means that output includes confidence interval with "con.level=XXX" 
wilcox.test(x, mu=163.5, alternative = "greater", conf.int = T,conf.level=0.95)

# ex1
x<-c(9, 12, 18, 14, 12, 14, 12, 10, 16, 11, 9, 11, 13, 11, 13, 15, 13, 14)
SIGN.test(x, md=12, alternative = "two.sided", conf.level = 0.95)

# ex2
x<-c(2.4, 2.3, 3.1, 2.2, 2.3, 1.2, 1, 2.4, 1.7, 1.1, 4.2, 1.9, 1.7, 3.6, 1.6, 2.3)
SIGN.test(x, md=2.5, alternative = "two.sided", conf.level = 0.95)

# ex3
62-29
x<-c(6.8, 5.7, 6.9, 5.3, 4.1, 9.8, 1.7, 7, 2.1, 19, 18.9, 16.9, 10.4, 44.1,
     2.9, 2.4, 4.8, 18.9, 2.8, 7.9)
SIGN.test(x, md=6.2, alternative = "less", conf.level = 0.95)

# ex4
x<-c(8.3, 9.5, 9.6, 8.75, 8.4, 9.1, 9.25, 9.8, 10.05, 8.15, 10, 9.6, 9.8, 9.2, 9.3)
SIGN.test(x, md=8.41, alternative = "greater", conf.level = 0.95)

# ex5
x<-c(35, 19, 40, 35, 51,41, 27, 23, 39, 21, 41, 31, 46, 51, 34, 37, 36, 55, 52, 32)
SIGN.test(x, md=45, alternative = "less", conf.level = 0.95)

# ex6
x<-c(28.5, 25.2, 28.7, 41, 29.1, 32.3, 27.7, 39.9, 26.8, 28.8)
sort(x)
5%/%2

# User_defined function to find center location using walsh mean statistics
walsh<-function(x){
  n<-length(x)
  N<-n*(n+1)/2
  w.mean<-NULL
  x<-sort(x)
  for (i in 1:length(x)){
    for (j in 1:length(x)){
      if (i<=j){
        w.mean<-c(w.mean, (x[i]+x[j])/2)
      }
    }
  }
  return(w.mean)
}
walsh(x)

wilcox.test(x, mu=28.95, conf.int = T, conf.level = 0.95)
dotplot(x)
# ex7
x<-c(19, 31, 1.3, 1.5, 43, 14, 14)
median(walsh(x))
sort(x)
wilcox.test(x, mu=16.375, alternative = "two.sided", conf.int = T, conf.level=0.9)

# ex8
x<-c(1.53, 1.47, 0.93, -0.02, 0.46, 1.33, 0.98, 1.96, -0.41, -1.23)
wilcox.test(x, mu=0, alternative = "two.sided", conf.int = T, conf.level = 0.95)

# ex9
x<-c(1, 10.3, 16.7, 38.4, 2.4, 2.6, 8.9, 36.3, 27.1, 3.8, 1.9, .9, .4, 9.2, 3)
x-4.8
wilcox.test(x, mu=4.8, alternative = "two.sided", conf.int = T, conf.level = 0.99)

# ex10
x<-c(5, 3.9, 5.2, 5.5, 2.8, 6.1, 6.4, 2.6, 1.7, 4.3)
x-3.7
wilcox.test(x, mu=3.7, alternative = "greater", conf.int = T, conf.level = 0.95)

# ex11
x<-c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7, 156.6, 174.5,
     184.4, 165.2, 147.8, 177.8, 160.1, 160.5)
x-160
length(x)
wilcox.test(x, mu=160, alternative = "greater", conf.int = T, conf.level = .95)

# ex12
x<-c(81.3001, 81.3015, 81.3006, 81.3011, 81.2997, 81.3005, 81.3021)
x-81.3035
wilcox.test(x, mu=81.3035, alternative = "two.sided", conf.int = T, conf.level = 0.95)


# ex13
x<-c(0.32, 0.21, 0.28, 0.15, 0.08, 0.22, 0.17, 0.35, 0.2, 0.31, 0.17, 0.11)
median(x)
SIGN.test(x, md=0.205, alternative = "two.sided", conf.int = T, conf.level = 0.95)

# ex14
x<-c(.07, .69, 1.74, 1.9, 1.99, 2.41, 3.07, 3.08, 3.1, 3.53, 3.71, 4.01, 8.11, 8.23, 9.1, 10.16)
median(x)
SIGN.test(x, md=3.4, alternative = "two.sided", conf.int = T, conf.level = 0.9)

# ex15
x<-c(13, 6, 6, 12, 12, 10, 9, 11, 14, 8, 7, 16, 15, 8, 7)
x-11.6
sort(x)
wilcox.test(x, mu=11.6, alternative = "two.sided", conf.int=T, conf.level = .95)
median(walsh(x))
wilcox.test(x, mu=10, alternative = "two.sided", conf.int = T, conf.level = .05)


###CH.3 two-sample location parameter problems###
x<-c(18,14.5,13.5,12.5,23,24,21,17,18.5,9.5)
y<-c(27,34,20.5,29.5,20,26.5,22,24.5,35.5)
var.test(x,y)
t.test(y,x,var.equal = T, alternative = "greater")
wilcox.test(y,x,alternative = "greater")

# Wilcoxon rank sum test is robust to outliers
x<-c(18,14.5,13.5,12.5,23,24,21,17,18.5,9.5)
y<-c(27,34,20.5,29.5,20,26.5,22,24.5,135.5)
var.test(x,y)
t.test(y,x,var.equal = F, alternative = "greater")
wilcox.test(y,x,alternative = "greater")

a<-c(90,86,72,65,44,52,46,38)
b<-c(80,70,62,53,87,44,42,35,46)
wilcox.test(a,b,alternative = "two.sided")
# when there are ties, wilcox.test calculate statistics using large sample approximation
# exact =F : removes warning message
wilcox.test(a,b,alternative = "two.sided", exact=F)

# paired test using t.test & wilcox.test
x<-c(60,64,55,75,71,68,58)
y<-c(58,69,58,78,75,68,67)
t.test(y,x,alternative = "greater", paired = T)
wilcox.test(y-x, alternative = "greater", exact=F)
# with paired sample, wilcox.test is signed rank test

# ex1
y<-c(2.38,4.19,1.39,3.73,2.86,1.21)
x<-c(4.67,5.38,3.89,4.67,3.58,4.96,3.98)
sort(c(x,y))
wilcox.test(y,x,alternative = "less", exact=F)


# ex2
y<-c(28,53,39,27,41,68,27,28,45,48,65,78)
x<-c(32,35,61,43,82,44,78,38,85,63,46,30,47,57)
length(y)
length(x)
sort(c(y,x))
wilcox.test(y,x,alternative = "two.sided")

# ex3
y<-c(102,86,98,109,92)
x<-c(81,165,97,134,92,87,114)
wilcox.test(y,x,alternative = "two.sided", conf.int = T, conf.level = .95)
sort(c(x,y))
sort(x)
sort(y)

# ex4
x<-c(167,149,137,178,179,155,164,104,151,150)
y<-c(98,127,140,103,116,105,100,95,131)
sort(x)
sort(y)
wilcox.test(y,x,alternative = "two.sided",conf.int = T, conf.level = .9)

# ex5
x<-c(6.6,5.8,7.8,5.7,6,8.4,8.8,8.4,7.3,5.8,5.8,6.5)
y<-c(6.4,5.8,7.4,5.5,6.3,7.8,8.6,8.2,7,4.9,5.9,6.5)
wilcox.test(y,x,alternative="less")
wilcox.test(y,x,alternative="two.sided",conf.int = T, conf.level = 0.9)

# ex6
y<-c(14.6,15.8,16.4,14.6,14.9,14.3,14.7,17.2,16.8,16.1)
x<-c(15.5,17.9,15.5,16.7,17.6,16.8,16.7,16.8,17.2,18)
wilcox.test(y,x,alternative = "greater")

# ex7
y<-c(5.6,4.6,6.8,4.9,6.1,5.3,4.5,5.8,5.4,4.7)
x<-c(7.2,8.1,5.1,7.3,6.9,7.8,5.9,6.7,6.5,7.1)
wilcox.test(y,x,alternative='two.sided')

# ex8
x<-c(90.4,77.2,75.9,83.2,84,90.2,87.6,67.4,77.6,69.3,83.3,72.7)
y<-c(92.7,78.9,82.5,88.6,95,94.4,73.1,88.3,90.4,86.5,84.7,87.5)
wilcox.test(y,x,alternative = "greater")
wilcox.test(y,x, alternative = "two.sided", conf.int = T, conf.level=.95)

# ex9
x<-c(2.1,5.3,1.4,4.6,.9)
y<-c(1.9,.5,2.8,3.1)
wilcox.test(y,x,alternative="two.sided", conf.int=T, conf.level=.9)

# ex10
x<-c(158,149,160,155,164,138,163,159,165,145,150,161,132,155,146,159)
y<-c(164,158,163,160,172,147,167,169,173,147,156,164,133,161,154,170)
new_y<-y-5
wilcox.test(new_y,x,alternative = "greater")

# ex11
x<-c(66,80,69,52,75)
y<-c(71,82,68,56,73)
wilcox.test(y,x,alternative = "two.sided", conf.int = T,conf.level = .9)

# ex12
x<-c(463,462,462,456,450,426,418,415,409,402)
y<-c(523,494,461,535,476,454,448,408,470,437)
wilcox.test(y-x, alternative = "greater")
wilcox.test(y-x, alternatice="two.sided", conf.int = T, conf.level = .95)

# ex13
y<-c(140,90,125,130,95,121,85,97,131,110)
x<-c(130,87,110,132,96,120,86,90,129,100)
wilcox.test(y-x, alternative = "greater")
wilcox.test(y-x, alternative="two.sided", conf.int = T, conf.level = .9)

# ex14
x<-c(70,80,72,76,76,76,72,78,82,64,74,68,84)
y<-c(68,72,62,70,58,66,68,52,64,72,74,72,74)
wilcox.test(y-x, alternative = "less")
wilcox.test(y-x, alternative="two.sided", conf.int = T, conf.level = .95)

# ex15
x<-c(18,12,7,21,19,14,8,11,19,16,8,11)
y<-c(10,10,8,23,13,10,8,13,9,8,8,5)
wilcox.test(y-x, alternative="less")
wilcox.test(y-x, alternative="two.sided", conf.int = T, conf.level = .9)

###CH.4 two-sample scale parameter problems###

# ansary-bradley test : non-parametric scale parameter test
a<-c(10.7, 11.1,10.4,10.1,11.3)
b<-c(10.8, 10.5,11,10.9,10.9,10.8,10.8)
var.test(a,b,alternative = "two.sided")
ansari.test(a,b, alternative="two.sided")

a<-c(6.67,6.71,6.69,6.74,6.65,6.72)
b<-c(6.54,6.77,6.43,6.82,6.74,6.79)
ansari.test(a,b,alternative="less")

# ex1
a<-c(.95,.82,.78,.96,.71,.86,.99)
b<-c(.89,.91,.94,.91,.9,.89)
wilcox.test(b,a,alternative="two.sided")
ansari.test(b,a,alternative="less")

# ex2
a<-c(1.9,0.8,1.1,0.1,-0.1,3.4,5.5,1.6,4.6,2)
b<-c(0.7,-1.6,-0.2,-1.2,0,4.4,3.7,.8,0,3.4)
wilcox.test(b-a, alternative = "two.sided", conf.int = T, conf.level = 0.95)
new_b<-b--1.150067 
ansari.test(a,new_b, alternative="two.sided")

# ex3 moses test
# user_defined function to perform moses test
5%/%2
x[sample(length(x),length(x))]
moses.test<-function(x,y,k){
  m<-length(x); n<-length(y)
  x<-x[sample(m,m)]; y<-y[sample(n,n)]
  mm<-m%/%k; nn<-n%/%k
  x<-x[1:(mm*k)]; y<-y[1:(nn*k)]
  matx<-matrix(x,nrow=k,ncol=mm, byrow=T)
  maty<-matrix(y,nrow=k,ncol=nn, byrow=T)
  c<-numeric(mm); d<-numeric(nn)
  for (i in 1:mm){
    c[i]<-sum((matx[,i]-mean(matx[,i]))^2)
  }
  for (j in 1:nn){
    d[j]<-sum((maty[,j]-mean(maty[,j]))^2)
  }
  if (length(c)>=length(d)){
    return(wilcox.test(d,c,alternative = "two.sided"))
  }else{
    return(wilcox.test(c,d,alternative="two.sided"))
  }
}
x<-c(0.97,0.72,1,0.81,0.62,1.32,1.24,.99,.9,.74,.88,.94,1.16,.86,.85,.58,.57,.64,.98,1.09,.92,.78,1.24,1.18)
y<-c(0.48,0.71,.98,.68,1.18,1.36,.78,1.64)
moses.test(x,y,4)

# ex4
x<-c(6.6,5.8,5.4,5.1,5,4.3,3.9,3.3,2.4,1.7)
y<-c(11.7,9.5,9.4,8.7,8.2,7.7,7.4,7.4,7.1,6.9,6.8,6.3,5,4.2,4.1,2.2)
moses.test(x,y,5)

# ex5
x<-c(1.2,.2,.3,.9,4.2,0.9,.3,.7,.9,1.1,3,.9,2.3,1.3,.2,1.5,2.1,7.7,20,1.2,3.4,2.2,.1,4.3,.7,.7,1.3,9.8,
     .9,4.7,0,.4,21,12,4.2,2.7,1.7,.5,1,.9,2.1,.1,1.7,1,3.9,1,.5,.7,.2,.9,.9,.8,.5,1.5,1.1,1.1,1.6,1.5,4,4.7,.9)
y<-c(1.4,1.6,1.4,4.1,2.6,1.1,0.4,1.8,2.2,.3,1.3,1.7,1,1.2,1.4,.5,1.1,1.5,1.1,3.3,2.6,.7,.1,1.6,2.5,.7,1.7,.3,1.9,1,.5)
length(x);length(y)
moses.test(x,y,5)

# ex6
x<-c(3.42,3.54,3.21,3.63,3.22,3.8,3.7,3.2,3.75,3.31,3.86,4,2.86,2.92,3.59,2.91,3.77,2.7,3.06,3.3)
y<-c(3.5,4,3.43,3.85,3.84,3.21,3.58,3.94,3.48,3.76,3.87,2.93,4,3.37,3.72,4,3.06,3.92,3.72,3.91)
length(x)
length(y)
moses.test(x,y,4)

# ex7
x<-c(9,10,4,19,13,12,8,0,13,6,12,5,7)
y<-c(6,7,3,19,4,12,2,0,6,7,5,0,7)
wilcox.test(y,x,conf.int = T, conf.level = .95)
new_y<-y--3.999922 
ansari.test(new_y, x, alternative = "two.sided")

# ex8
x<-c(117.1,121.3,127.8,121.9,117.4,124.5,119.5,115.1)
y<-c(123.5,125.3,126.5,127.9,122.1,125.6,129.8,117.2)
wilcox.test(y,x,conf.int = T, conf.level = .95)
new_y<-y-4.65 
ansari.test(new_y,x,alternative = "less")


###CH.5 Distribution problems : Identify distribution's equality###
x<-c(16.7,24.7,36.5,46.2,43.2,25.9,17.4,18.1,24,22.4,18.8,39.8,37.6,35.1,18.2,19.3,27,35.8,42.1,22.4)
# "cut" : divide vector into subintervals
cut_x<-cut(x, c(min(x)-1,23.255,30,36.745,max(x)))
cut_x
y<-summary(cut_x) #return 'cut_x' as table
chisq.test(y)
# kolmogorov goodness-of-fit test
# Coutiouos about interpretation of test results
?ks.test
ks.test(x, "pnorm", mean=30, sd=10, alternative="two.sided")

x<-c(4.6,4.9,7.4,8.3,6.4,5,5,6.6,6.3,6.8,5.1,5.8,4.7,5.7,7.9,7.1)
y<-c(6.8,6,5.6,8.3,8.1,8.9,7.4,12.7,9.6,8.6,5.9,11.5,6.6,6.7,9.8,8.4)
ks.test(x,y,alternative = "greater")

# ex1 
x<-c(16.3,14.2,14.7,16,15.7)
ks.test(x, "dunif", 15, 18)

# ex2
x<-c(4,0,2,0,2,0,2,0)
ks.test(x, "dpois",1.5)

# ex3 : confidence band
x<-c(16.3,14.2,14.7,16,15.7)
densityplot(x)
x<-sort(x)
cutx<-cut(x, 0:40)
y<-summary(cutx)          
cumy<-cumsum(y)/5
cumy
plot(cumy, type="l")
lx<-numeric(40)
for (i in 1:40){
  lx[i]<-max(cumy[i]-.509,0)
}
lx
lines(lx, col="red")
ux<-numeric(40)
for (i in 1:40){
  ux[i]<-min(cumy[i]+0.509,1)
}
lines(ux, col="red")

# ex4
x<-c(4,0,2,0,2,0,2,0)
length(x)
cutx<-cut(x, -10:10)
y<-summary(cutx)
cumy<-cumsum(y)/8
plot(cumy, type="l")
ux<-numeric(20)
lx<-numeric(20)
for (i in 1:20){
  ux[i]<-min(cumy[i]+.509,1)
  lx[i]<-max(cumy[i]-.509,0)
}
lines(ux, col="red")
lines(lx, col="red")

# ex5
x<-c(.6,.8,.8,1.2,1.4)
y<-c(1.3,1.3,1.8,2.4,2.9)
ks.test(x,y,alternative = "greater")

# ex6
x<-c(82,74,87,86,75)
y<-c(88,77,91,88,94,93,83,94)
ks.test(y,x,alternative = "two.sided")

# ex7
x<-c(437,358,72,43,107,223,60,72,54,35,70,20,34,24,24,51,23)
length(x)
y<-c(18,16,18)
ks.test(x,y,alternative="two.sided")

# ex8
x<-c(20.3,22.53,25.7,13.23,29.67,24.46,26.07,19.35,17.81,16,13.5,32.9)
y<-c(10.56,28.13,19.94,11.03,8.93,12.95,21.14,32.5,10.9)
ks.test(y,x,alternative="two.sided")


###CH.6 One-way ANOVA###
A<-c(56,60,57,64)
B<-c(48,61,49,53)
C<-c(52,50,44,46)
x<-c(A,B,C)
treat<-as.factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C))))
aov1<-aov(x~treat)
summary(aov1)
# Tukey multiple comparison
TukeyHSD(aov1)
# kruskal willis test
kruskal.test(x, treat)
kruskalmc(x, treat)
?kruskal.test
# Jonckheere test
install.packages("clinfun")
library(clinfun)
A<-c(10,12,14,17)
B<-c(13,15,16,18,21)
C<-c(11,26,27,28,29,30)
treat<-c(rep(1, length(A)), rep(2, length(B)), rep(3, length(C)))
x<-c(A,B,C)
jonckheere.test(x, treat, "increasing")

# ex1
A<-c(772, 764, 600, 564)
B<-c(792, 612, 592)
C<-c(752, 680, 624, 580, 572)
x<-c(A,B,C)
treat<-as.factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C))))
aov2<-aov(x~treat)
summary(aov2)
TukeyHSD(aov2)
kruskal.test(x, treat)

# ex2
A<-c(72, 65, 67, 75, 62, 73)
B<-c(55, 59, 68, 70, 53, 50)
C<-c(64, 74, 61, 58, 51, 69)
br<-as.factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C))))
x<-c(A,B,C)
aov2<-aov(x~br)
summary(aov2)
TukeyHSD(aov2)
kruskal.test(x, br)

# ex3
A<-c(114, 108, 110, 113, 113)
B<-c(111,109,112,114,115)
C<-c(107, 106, 116, 111, 102)
D<-c(105, 98, 102, 101, 96)
x<-c(A, B, C, D)
bus<-as.factor(c(rep("A",length(A)), rep("B", length(B)), rep("C", length(C)), rep("D", length(D))))
aov3<-aov(x~bus)
summary(aov3)
TukeyHSD(aov3)
kruskal.test(x, bus)

# ex4
A<-c(257, 205, 206, 231, 190, 214, 228, 203)
B<-c(201, 164, 197, 185)
C<-c(248, 265, 187, 220, 212, 215, 281)
D<-c(202, 276, 207, 230, 227)
treat<-as.factor(c(rep("A",length(A)), rep("B", length(B)), rep("C", length(C)), rep("D", length(D))))
x<-c(A, B, C, D)
aov4<-aov(x~treat)
summary(aov4)
TukeyHSD(aov4)
kruskal.test(x, treat)

# ex6
A<-c(99, 114, 116, 127, 146)
B<-c(111, 125, 143, 148, 157)
C<-c(113, 139, 149, 160, 184)
x<-c(A, B, C)
treat<-c(rep(1, length(A)), rep(2, length(B)), rep(3, length(C)))
jonckheere.test(x, treat, "increasing")

# ex7
A<-c(71, 57, 85, 67, 66, 79)
B<-c(76, 94, 61, 36, 42, 49)
C<-c(80, 104, 101, 90, 93, 85)
treat<-c(rep(1, length(A)), rep(2, length(B)), rep(3, length(C)))
x<-c(A, B,C)
jonckheere.test(x, treat, "increasing")

# ex8
A<-c(17, 20, 24, 34, 34, 38)
B<-c(23, 25, 27, 34, 38, 47)
C<-c(22, 23, 26, 32, 34, 34, 36, 38,38, 42,48, 50 )
x<-c(A, B, C)
treat<-c(rep(1, length(A)), rep(2, length(B)), rep(3, length(C)))
jonckheere.test(x, treat, "increasing")

# ex12
A<-c(82, 127, 53, 89, 81)
B<-c(32, 24, 16, 22, 30, 27, 30)
C<-c(117, 63, 72, 96, 117, 45)
x<-c(A, B, C)
treat<-as.factor(c(rep("A",length(A)), rep("B", length(B)), rep("C", length(C))))
aov5<-aov(x~treat)
summary(aov5)
TukeyHSD(aov5)
kruskal.test(x, treat)
install.packages("sp")
install.packages('rgdal',repos="http://www.stats.ox.ac.uk/pub/RWin")
install.packages('pgirmess')
# Using kruskalmc for multiple comparison
library(pgirmess)
kruskalmc(x, treat)

# ex13
A<-c(7.6,7.7,7.5,7.8,7.6,7.3,7.1,8,7.5,8)
B<-c(8.9, 8.2,8.1,8,8.6,8.6,8.6,8.4)
C<-c(8, 8.8, 8.7, 8.6, 9, 8.8, 8.5)
D<-c(9.9, 9.1, 9.8, 9.8, 9.9, 9.6, 9.2, 9.8)
x<-c(A, B,C, D)
treat<-as.factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C)), rep("D", length(D))))
kruskal.test(x, treat)
kruskalmc(x, treat, probs=0.15)
?kruskalmc
treat2<-as.factor(c(rep("A", length(A)), rep("B", length(B)+length(C)+length(D))))
kruskalmc(x, treat2, probs=0.15)
 
##CH.7 Two-way ANOVA : Freidman test, page test###
A<-c(4000, 1600, 1600, 1200, 840, 352, 224, 200, 184)
B<-c(3210, 1040, 647, 570, 445, 156, 155, 99, 70)
C<-c(6120, 2410,2210, 2060, 1400, 249, 224, 208, 227)
x<-c(A,B,C)
treat<-factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C))))
block<-factor(c(rep(1:9, 3)))
aov6<-aov(x~treat+block)
summary(aov6)

#friedman.test 
friedman.test(x, treat, block)
?friedman.test

#page.trend.test
install.packages("crank")
library(crank)
tr1<-c(7.46, 7.68, 7.21)
tr2<-c(7.11, 7.57, 7.8)
tr3<-c(7.76, 7.73, 7.74)
tr4<-c(8.14, 8.15, 7.87)
tr5<-c(7.63, 8, 7.93)
x<-cbind(tr1, tr2, tr3, tr4, tr5)
page.trend.test(x, rank=F)
# data should be matrix (row : obs, col : treatment), option 'rank=F' means that raw data is not a rank.

# ex1
A<-c(30.4, 33.9, 32.7, 34.9, 31.9, 35.4)
B<-c(28.8, 25.5, 27.3, 29.3, 27.5, 28.3)
C<-c(33, 32.7, 34.5, 36, 36.5, 34.2)
D<-c(31.8, 33.5, 34.5, 33.8, 34.5, 36)
x<-c(A, B, C, D)
treat<-factor(c(rep("A", length(A)), rep("B", length(B)), rep("C", length(C)), rep("D", length(D))))
block<-factor(rep(1:6, 4))
friedman.test(x, treat, block)

# ex2
A<-c(-.08, .21, .5, .14)
B<-c(.01, .17, -.11, .07)
C<-c(.06, .19, .34, .14)
x<-c(A, B, C)
treat<-factor(c(rep("A",length(A)), rep("B", length(B)), rep("C", length(C))))
block<-factor(rep(1:4, 3))
friedman.test(x, treat, block)

# ex3
A<-c(3.5, 3.7, 1.6, 2.5, 2.8, 2, 5.9, 2.5)
B<-c(5.9, 8.1, 8.1, 8.6, 8.1, 5.9, 9.5, 7.9)
C<-c(13.9, 12.6, 8.1, 6.8, 14.3, 4.2, 14.5, 7.9)
x<-c(A, B, C)
treat<-factor(c(rep("A", length(A)), rep("B", length(B)),rep("C", length(C))))
block<-factor(rep(1:8, 3))
friedman.test(x, treat, block)

# ex5
x<-cbind(A, B, C)
page.trend.test(x, rank=F)

# ex4
sp<-c(92, 9, 98, 19, 21, 58, 42)
su<-c(112, 11, 109, 26, 22, 71, 49)
fa<-c(94, 10, 92, 19, 23, 51, 44)
wi<-c(77, 12, 81, 18, 24, 62, 41)
x<-cbind(sp, su, fa, wi)
?page.trend.test
page.trend.test(x, rank=F)

# ex6
mg5<-c(2.6, 2.5, 3, 2.8, 3.2, 3.3, 3.5)
mg10<-c(2.7, 2.5, 2.9, 3.1, 3.4, 3.4, 3.5)
mg25<-c(3.4, 2.9, 3.3, 3.7, 3.9, 4, 4.6)
length(mg25)
x<-cbind(mg5, mg10, mg25)
page.trend.test(x, rank=F)

# ex7
rat<-c(1.5, 1.1, 1.8, 1.9, 4.3, 2, 8.4, 6.6, 2.4, 6.5, 2.6, 6.5)
rab<-c(1.7, 1.5, 8.1, 1.3, 4, 4.6, 4, 5.1, 2.5, 6.9, 2.5, 6.8)
cat<-c(.3, 1, 3.6, 0, .6, 5.5, 1, 3.1, .1, 1.6, 4.3, 1)
x<-c(rat, rab, cat)
treat<-factor(c(rep("rat", length(rat)), rep("rab", length(rab)), rep("cat", length(cat))))
block<-factor(rep(1:12, 3))
# multiple comparison using friedman test : "friedmanmc"
friedmanmc(x, treat, block)

# ex8
s<-c(22.6, 53.1, 8.3, 21.6, 13.3, 37, 14.8, 14.8)
ho<-c(32.1, 57.6, 10.5, 23.6, 11.9, 54.6, 21, 20.3)
ha<-c(22.7, 53.2, 9.7, 19.6, 13.8, 47.1, 13.1, 23.6)
d<-c(22.5, 53.7, 10.8, 21.1, 13.7, 39.2, 13.7, 16.3)
x<-c(s, ho, ha, d)
treat<-factor(c(rep("stable", length(s)), rep("horror", length(ho)), rep("happy", length(ha)),rep("des", length(d))))
block<-factor(rep(1:8, 4))
friedmanmc(x, treat, block)

### CH.8 Independence & spearman correlation coefficient ###
x<-c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y<-c(2.6, 3.1, 2.5, 5, 3.6, 4, 5.2, 2.8, 3.8)
cor.test(x,y,method="kendall", alternative = "greater")
# Kendall's tau is just "method ="kendall"
# In output, T denote concordant pairs

x<-c(141.8, 140.2, 131.8, 132.5, 135.7, 141.2, 143.9, 140.2, 140.8, 131.7, 130.8, 135.6, 143.6, 133.2)
y<-c(89.7, 74.4, 83.5, 77.8, 85.8, 86.5, 89.4, 89.3, 88, 82.2,84.6, 84.4, 86.3, 85.9)
cor.test(x,y,method="spearm", alternative = "greater")
# spearman's rho is just "method=spearm"

# ex1
x<-c(2,1,3,5,4,8,7,6)
y<-c(1,2,4,5,7,6,8,3)
cor.test(x,y,method="kendall", alternative="greater")
cor.test(x,y,method="spearm", alternative="greater")

# ex2
x<-c(20, 17, 15, 19, 23, 14, 27, 17, 18, 15, 15, 23, 21, 16, 12, 19, 18, 19, 16, 17, 26, 21)
y<-c(90, 94, 100, 103, 103, 106, 108, 109, 109, 112, 112, 113, 114, 118, 119, 120, 124, 132, 133, 141, 155, 157)
cor.test(x,y,method="kendall", alternative = "greater")
cor.test(x,y,method="spearm", alternative="greater")

# ex3
x<-c(147, 158, 131, 142, 183, 151, 196, 129, 155, 158)
y<-c(122, 128, 125, 123, 115, 120, 108, 143, 124, 123)
cor.test(x,y,method="kendall", alternative="two.sided")
cor.test(x,y,method="spearm", alternative="two.sided")

# ex4
x<-1:13
y<-c(6.1, 7.5, 7.7, 5.9, 5.2, 6.1, 5.3, 4.5, 4.9, 4.6, 3, 4, 3.7)
cor.test(x,y,method="kendall", alternative = "less")
cor.test(x,y,method="spearm", alternative="less")

# ex5
x<-c(141.8, 140.2, 131.8, 132.5, 135.7, 141.2, 143.9, 140.2, 140.8, 131.7, 130.8, 135.6, 143.6, 133.2)
y<-c(89.7, 74.4, 83.5, 77.8, 85.8, 86.5, 89.4, 89.3, 88, 82.2, 84.6, 84.4, 86.3, 85.9)
cor.test(x,y,method="kendall", alternative="greater")

# ex6
x<-c(86, 71, 77, 68, 91, 72, 77, 91, 70, 71, 88, 87)
y<-c(88, 77, 76, 64, 96, 72, 65, 90, 65, 80, 81, 72)
cor.test(x,y,method="spearm", alternative="greater")


### Ch.9 regression analysis ###
# parametric regression : 'lm'
# Non-parametric regression : 'zyp.sen' in package 'zyp'
x<-c(3,4,5,6,7)
y<-c(4.8, 6, 7.2, 7.8, 8.2)
reg1<-lm(y~x)
summary(reg1)

install.packages("zyp")
library(zyp)
reg2<-zyp.sen(y~x)
reg2

# ???????????? ????????????

ci<-confint.zyp(reg2)
ci

# Theil test , NULL.H : beta=0
theil1<-Kendall(x,y)
summary(theil)

# ex1
reg3<-zyp.sen(y~x)
reg3
confint.zyp(reg3)
theil2<-Kendall(y,x)
summary(theil2)
summary(theil1)

# ex2
x<-c(4,5,6,7,10)
y<-c(8,7,10,15,20)
reg4<-zyp.sen(y~x)
reg4

# ex3
x<-c(200, 300, 400, 400, 500, 600)
y<-c(10000, 12000, 20000, 25000, 25000, 30000)
reg5<-zyp.sen(y~x)
reg5

# ex4
x<-c(2,3,4,5,6,11)
y<-c(20, 25, 30, 31, 34, 40)
cor.test(x,y,method="kendall", alternative = "greater")

reg6<-zyp.sen(y~x)
reg6

# ex5
x<-c(0, 5000, 10000, 15000, 20000, 25000, 30000, 100000)
y<-c(0.924, 0.988, 0.992, 1.118, 1.133, 1.145,1.157,1.357)
cor.test(x,y,method="kendall", alternative = "greater")
reg7<-zyp.sen(y~x)
reg7

# ex6
confint.zyp(reg7, level=0.938)

# ex7
x<-c(3.81, 2.1, .79, 1.99, 1.03, 2.07, .74, 3.88, 1.43, .41)
y<-c(1.9, 1.03,.44,1.18, .62, 1.29, .39, 2.3, .93,.23)
cor.test(x,y,method="kendall")
reg8<-zyp.sen(y~x)
reg8

# ex8
confint.zyp(reg8, level=0.928)

# ex9 : Hollander's test
install.packages("NSM3")
library(NSM3)
male<-matrix(c(85,86,83,92,81,76,65,99,78,57,80,83,68,69,54,92),ncol=2)
female<-matrix(c(75,57,87,80,91,99,81,93,70,37,76,59,84,73,69,74),ncol=2)
female
X<-list(male, female)
sen.adichie(X, example=F, r=3)

# ex10
heart<-matrix(c(177,159,154,160,157,169,160,180,158,167,61,60,62,63,67,63,45,87,47,58),ncol=2)
health<-matrix(c(180,177,167,170,171,163,165,164,168,185,79,62,62,61,66,56,52,54,58,84),ncol=2)
X<-list(heart, health)
sen.adichie(X, example=F, r=4)

