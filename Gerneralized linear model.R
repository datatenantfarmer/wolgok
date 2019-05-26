### CH2. group comparison

# prob 1
cur<-c(48.2, 54.6, 58.3, 47.8, 51.4, 52, 55.2, 49.1, 49.9, 52.6)
new<-c(52.3, 57.4, 55.6, 53.2, 61.3, 58, 59.8, 54.8, 51.2, 46.2)
var.test(cur, new, alternative = "two.sided")

# prob 2
cell1<-c(9, 9.4, 4.7, 4.8,8.9, 4.9, 8.4, 5.9, 6.3, 5.7, 5, 3.5, 7.8, 10.4, 8,8,8.6,7,6.8,7.1
         , 5.7, 7.6, 6.2, 7.1, 7.4, 8.7, 4.9, 7.4, 6.4, 7.1, 6.3, 8.8, 8.8, 5.2, 7.1, 5.3, 
         4.7, 8.4, 6.4, 8.3)
cell2<-c(12.6, 14.6, 16.2, 23.9, 23.3, 17.1, 20, 21, 19.1, 19.4, 16.7, 15.9, 15.8, 16, 17.9,
         13.4, 19.1, 16.6, 18.9, 18.7, 20, 17.8, 13.9,22.1,13.9, 18.3, 22.8, 13, 17.9, 15.2, 17.7, 15.1, 16.9, 
         16.4, 22.8, 19.4, 19.6, 18.4, 18.2, 20.7)
var.test(cell1, cell2, alternative = "two.sided")
t.test(cell1, cell2, alternative = "less")

# prob 3
pre<-c(.4,.4,.4,.4,.5,.5,.5,.5,.5,.6,.7,.7,.8,.9,.9,1,1,2)
post<-c(.4,.5,.5,.9,.5,.5,.5,.5,.5,.6,1.1,1.2,.8,1.2,1.9,.9,2,3.7)
t.test(pre, post, paired = T, alternative = "two.sided")

# prob 4
score1<-c(104,116,84,77,61,84,81,72,61,97,84)
score2<-c(108,118,89,71,66,83,88,76,68,96,81)
t.test(score1, score2, paired = T, alternative = "two.sided")

# prob 5
low<-c(22.2,97.8,29.1,37,35.8,44.2,82,56,9.3,19.9,39.5,12.8)
mid<-c(15.1,23.2,10.5,13.9,9.7,19,19.8,9.1,30.1,15.5,10.3,11)
high<-c(10.2,11.3,11.4,5.3,14.5,11,13.6,33.4,25,27,36.3,17.7)
data<-c(low, mid, high)
treat<-c(rep("low",12), rep("mid",12), rep("high",12))
treat<-as.factor(treat)
mod1<-aov(data~treat)
summary(mod1)

# prob 6
non<-c(25,120,90,109,82,40,68,84,124,77,140)
pre<-c(62,73,60,77,52,115,82,52,105,143,80)
smo<-c(96,107,63,134,140,103,158,131,76,69,69)
data<-c(non, pre, smo)
state<-c(rep("non",11), rep("pre",11), rep("smo",11))
state<-as.factor(state)
mod2<-aov(data~state)
summary(mod2)

# prob 7
library(agricolae)
data<-c(7,5.3,4.9,8.8,9.9,5.7,7.6,8.9,8.5,4.7,5.5,8.1,5.1,3.5,2.8,3.3,10.3,7.7,8.4,9.1)
exc<-c(rep("A",4), rep("B",4), rep("C", 4), rep("D",4),rep("E",4))
diet<-rep(1:4, 5)
exc<-as.factor(exc); diet<-as.factor(diet)
mod3_1<-aov(data~exc)
mod3_2<-aov(data~diet)
summary(mod3_1)
summary(mod3_2)
scheffe.test(mod3_1, "exc", group = T, console = T)
scheffe.test(mod3_2, "diet", group = T, console = T)

# prob 8
motiv<-c(58,68,60,68,64,62,70,65,80,69,67,78,68,81,70)
class<-c(rep("non",5),rep("low",5), rep("very low", 5))
method<-rep(1:5, 3)
class<-as.factor(class); method<-as.factor(method)
mod4_1<-aov(motiv~class)
mod4_2<-aov(motiv~method)
summary(mod4_1)
summary(mod4_2)
HSD.test(mod4_1, "class", group=T, console = T)
HSD.test(mod4_2, "method", group=T, console = T)

# prob 9
mari<-c(25,28,22,18,23,19,17,24,19,28,32,30,16,24,20,18,22,20,25,35,30,14,16,15,10,8,12)
age<-c(rep("15-19", 9), rep("20-24",9), rep("25-29",9))
use<-rep(c(rep("no", 3), rep("sometimes",3), rep("everyday",3)),3)
age<-as.factor(age); use<-as.factor(use)
mod5<-aov(mari~age*use)
summary(mod5)

# prob 10
ami<-c(21.5,14.5,16,19.6,17.4,20.3,20.9,15,18.5,22.8,17.8,19.3,
       14.8,12.1,14.4,15.6,11.4,14.7,13.5,12.7,13.8,16.4,14.5,12)
species<-rep(c("Spe1","Spe2","Spe3"),8)
gender<-c(rep("M",12), rep("F",12))
species<-as.factor(species)
gender<-as.factor(gender)
mod6<-aov(ami~species*gender)
summary(mod6)

### CH3

# prob 1
mat1<-matrix(c(5,21,8,82),2,2,byrow = T)

RR<-function(mat, alpha){
  val1<-mat[1,1]/sum(mat[1,])
  val2<-mat[2,1]/sum(mat[2,])
  val<-val1/val2
  var<-(1-mat[1,1]/sum(mat[1,]))/mat[1,1]+(1-mat[2,1]/sum(mat[2,]))/mat[2,1]
  z<-qnorm(alpha/2, 0,1)
  lower<-val*exp(z*sqrt(var)); upper<-val*exp(-z*sqrt(var))
  return(list(RR=val, alpha_CI_L=lower, alpha_CI_U=upper))
}

OR<-function(mat, alpha){
  val=mat[1,1]*mat[2,2]/(mat[1,2]*mat[2,1])
  z<-qnorm(alpha/2,0,1)
  var<-sum(1/mat)
  lower<-val*exp(z*sqrt(var)); upper<-val*exp(-z*sqrt(var))
  return(list(OR=val, alpha_CI_L=lower, alpha_CI_U=upper))
}

OR(mat1, 0.05)
RR(mat1, 0.05)

# prob 2
mat2<-matrix(c(273,2914-273,716,7976-716),2,2,byrow=T)
OR(mat2, 0.05)

# prob 3
mat3<-matrix(c(49,12,24,9,2,29), nc=2, nr=3)
chisq.test(mat3)
fisher.test(mat3)

# prob 4
mat4<-matrix(c(23,4,10,10,14,35), nc=3, nr=2, byrow=T)
chisq.test(mat4)

# prob 5
mat5<-matrix(c(21,2,15,3),2,2,byrow=T)
fisher.test(mat5)

# prob 6
mat6<-matrix(c(1,5,8,2),2,2,byrow=T)
fisher.test(mat6)

# prob 7
mat7<-matrix(c(794,150,86,570),2,2,byrow=T)
mcnemar.test(mat7)

# prob 8
install.packages("vcdExtra")
library("vcdExtra")
?CMHtest

# prob 9

# prob 10
metric<-function(mat, prob){
  sen<-mat[1,1]/sum(mat[,1])
  spe<-mat[2,2]/sum(mat[,2])
  ppt<-sen*prob/(sen*prob+(1-spe)*(1-prob))
  pnt<-spe*(1-prob)/((1-sen)*prob+spe*(1-prob))
  return(list(Sensitivity=sen, Specificity=spe, ppt=ppt, pnt=pnt))
}
mat8<-matrix(c(302, 80, 179, 372),2,2,byrow=T)
metric(mat8, 0.1)


### CH4 logistic regression / log lineaer model

# prob 1
arrest<-c(rep(1, 42+17+33+53), rep(0, 109+75+175+359))
home<-c(rep("no",42+17),rep("yes",33+53),rep("no",109+75),rep("yes", 175+359))
exp<-c(rep("yes",42), rep("no", 17), rep("yes",33), rep("no",53), rep("yes",109),rep("no", 75),rep("yes",175),rep("no",359))
home<-as.factor(home); exp<-as.factor(exp)
model1<-glm(arrest~home+exp, family = "binomial")
summary(model1)

# prob 2
cand<-c(rep(1, 1+13+44+155+92+100+18+2+1+2), rep(0, 12+57+71+146+61+41+8+6+16+23+31+8+7+4))
race<-c(rep("white", 1+13+44+155+92+100+18), rep("black", 2+1+2), rep("white", 12+57+71+146+61+41+8), rep("black",6+16+23+31+8+7+4))
idea<-c(1, rep(2,13), rep(3, 44), rep(4,155), rep(5, 92), rep(6, 100), rep(7, 18)
        , rep(3,2),4,6,6, rep(1, 12), rep(2, 57), rep(3, 71), rep(4,146), rep(5,61), rep(6,41), rep(7,8),
        rep(1,6), rep(2, 16), rep(3, 23), rep(4, 31), rep(5,8), rep(6,7), rep(7, 4))
race<-as.factor(race)
model2<-glm(cand~race+idea, family = binomial)
summary(model2)

# prob 3
f1<-c(23, 7, 5, 13, 5,8,16,17)
f2<-c(4,0,11,8,11,7,19,1)
f3<-c(2,1,1,6,2,6,1,0)
f4<-c(2,3,0,1,1,3,2,1)
f5<-c(8,5,3,0,5,5,3,3)
lake<-rep(c("H","O","T","G"),2)
length<-rep(c("small","big"),4)
lake<-as.factor(lake); length<-as.factor(length)

library("nnet")
install.packages("VGLM")

model3<-multinom(cbind(f1,f2,f3,f4,f5)~lake+length)
model3<-vglm(cbind(f1,f2,f3,f4,f5)~lake+length, family=multinomial(refLevel = "f1"))
summary(model3)

# prob 4
diag<-c(rep(1, 91+150+109), rep(2, 90+200+198), rep(3, 51+155+172))
age<-c(rep("<45", 91), rep("45-59", 150), rep("60+", 109), rep("<45", 90), rep("45-59", 200), rep("60+", 198),
       rep("<45", 51), rep("45-59", 155), rep("60+", 172))
age<-as.factor(age)
model4<-vglm(diag~age, family = cumulative(parallel = T))
summary(model4)

# prob 5
mag<-c(rep(4,12),rep(3,12), rep(2,7), rep(1,9))
status<-c(rep(1,4),0,1,0,1,1,1,0,0,1,0,1,0,1,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0)
event<-c(1,9,4,3,2,0,1,3,3,7,1,2,5,6,3,1,8,2,5,5,9,3,3,1,0,4,3,9,6,4,3,8,2,7,5,4,4,8,8,9)
model5<-vglm(mag~status+event, family = cumulative(parallel = T))
summary(model5)

# prob 6
count<-c(483,477,1101,1121)
cha<-c("A","B","A","B")
exc<-c("yes","yes","other","other")
cha<-as.factor(cha)
exc<-as.factor(exc)
model6_1<-glm(count~cha*exc, family = "poisson")
model6_2<-glm(count~cha+exc, family = "poisson")
summary(model6_2)
anova(model6_2, model6_1, test="Chisq")

# prob 7
count<-c(659, 270, 532, 347, 432, 532, 269, 552)
belt<-c(rep("yes",4), rep("no",4))
inj<-rep(c(rep("yes",2), rep("no",2)),2)
belt<-as.factor(belt)
inj<-as.factor(inj)
model7<-glm(count~belt*inj, family = "poisson")
model7_1<-glm(count~belt+inj, family = "poisson")
summary(model7_1)
anova(model7_1, model7, test="Chisq")
model7_2<-glm(count~inj, family = "poisson")
anova(model7_2, model7_1, test="Chisq")

# prob 8 : cautious about large sample -> examine odds ratio
count<-c(304,38,92,64,665,85,174,113,894,93,379,321,720,84,433,297)
lent<-rep(c("yes","yes","no","no"),4); lent<-as.factor(lent)
status<-rep(1:4, each=4); status<-as.factor(status)
model8<-glm(count~lent*status, family = poisson)
summary(model8)
model8_1<-glm(count~lent+status, family = poisson)
summary(model8_1)

# prob 9 : cautious about large sample -> examine odds ratio
count<-c(34,64,49,135,29,61,31,118,40,81,37,142,37,65,32,64,24,40,11,37,6,15,0,3,3,7,4,2)
length(count)
gender<-rep(c("M","M","F","F"),7); gender<-as.factor(gender)
length(gender)
time1<-rep(1:7, each=4); time1<-as.factor(time1)
model9<-glm(count~gender*time1, family = poisson)
summary(model9)
model9_1<-glm(count~gender+time1, family = poisson)

time2<-c(rep(1,8), rep(2,12), rep(3, 8)); time2<-as.factor(time2)
model9_2<-glm(count~gender*time2, family = poisson)
summary(model9_2)



