### Ch2 example

# t test - same variance / distinct variance
smo<-c(166,139,113,265,174,153,158,123,186,120,241,165,218,163,234,188)/10
non<-c(181,60,108,110,77,179,85,130,189)/10
var.test(smo, non, alternative = "two.sided")
t.test(smo, non)

# paired t- test
pre<-c(90,56,49,64,65,88,62,91,74,93,55,71,54,64,54)
post<-c(72,55,56,57,62,79,55,72,73,74,58,59,58,71,61)
t.test(post, pre, paired = T, alternative = "less")

# one way anova
den1<-c(755,343,820,345,170,460,325,440,380,360,400,450,415,410,
       225,400,435,360,365,900,300,385,215)
length(den1)
den2<-c(165,390,290,435,235,345,320,330,205,375,345,305,220,270,
        355,360,335,305,325,245,285,370,345,345,230,370,285,315,
        195,270,305,375,220)
den3<-c(380,510,315,565,715,380,390,245,155,335,295,200,105,105,245)
class<-c(rep(1,length(den1)), rep(2,length(den2)), rep(3, length(den3))); class<-as.factor(class)
den<-c(den1, den2,den3)
anova1<-aov(den~class)
summary(anova1)

library(agricolae)
HSD.test(anova1, "class", group=T, console = T)

duncan.test(anova1, "class", group = T, console=T)

# two way anova
cal<-c(1687,1618,1712,1683,1719,1586,1492,1563,1524,1480,1907,1877,1763,1699,1804,
       1720,1764,1789,1678,1672,3245,2871,3465,2879,2446,3054,3241,2897,2846,2965)/100
treat<-rep(1:3, each=10)
gender<-rep(c(rep("M",5),rep("F",5)),3)
treat<-as.factor(treat); gender<-as.factor(gender)
anova2<-aov(cal~treat*gender)
summary(anova2)

HSD.test(anova2, "treat", group=T, console = T)

### CH3 example
aspirin<-matrix(c(139,239,10898,10795),2,2)
RR(aspirin, 0.05)

strock<-matrix(c(73,141,18,196),2,2)
OR(strock, 0.05)

chisq.test(aspirin)

relation<-matrix(c(1,3,4,2),2,2)
fisher.test(relation, alternative = "less")

wedding<-matrix(c(23,18,7,12),2,2)
mcnemar.test(wedding, correct = T)

river<-matrix(c(231,32,27,54), 2,2,byrow=T)
metric(river,.75)
metric(river,.25)

xray<-matrix(c(22,51,8,1739),2,2,byrow=T)
metric(xray, .000093)

### CH4 example
yes<-c(65,100,56,80,29,78)
no<-c(18,13,38,15,9,22)
treat<-rep(LETTERS[1:3],2)
type<-rep(c("T","M"),3)
treat<-as.factor(treat)
type<-as.factor(type)
logit_ex<-glm(cbind(yes, no)~treat+type, family = binomial)
summary(logit_ex)

imp<-c(rep(0,6+19+7+10), rep(1,5+7+2), rep(2,16+6+5+1))
treat<-c(rep("A",6), rep("P",19), rep("A",7),rep("P",10),rep("A",5),rep("P",7),
         rep("A",2),rep("A",16),rep("P",6),rep("A",5),rep("P",1))
gender<-c(rep("F",6+19), rep("M",17),rep("F",12), rep("M",2),rep("F",6+16), rep("M",6))
treat<-as.factor(treat)
gender<-as.factor(gender)
logit_ex2<-vglm(imp~treat+gender, family = cumulative(parallel = FALSE))
logit_ex2_1<-vglm(imp~treat+gender, family = cumulative(parallel = TRUE))
logit_ex2_2<-vglm(imp~treat*gender, family = cumulative(parallel = FALSE))
summary(logit_ex2)
summary(logit_ex2_1)
summary(logit_ex2_2)
logit_ex2_1@df.residual
logit_ex2_2@df.residual

party<-c(rep("D",132+42+172+56), rep("R", 176+6+129+4), rep("I", 127+12+130+15))
race<-c(rep("W",132), rep("B",42),rep("W",172), rep("B",56),rep("W",176), rep("B",6),
        rep("W",129), rep("B",4),rep("W",127), rep("B",12),rep("W",130), rep("B",15))
gender<-c(rep("M",132+42), rep("F",172+56),rep("M",176+6), rep("F",133),rep("M",139), rep("F",145))
race<-as.factor(race)
gender<-as.factor(gender)
logit_ex3<-vglm(party~gender+race, family = multinomial(refLevel = "D"))
logit_ex3_1<-vglm(party~gender*race, family = multinomial(refLevel = "D"))
summary(logit_ex3)
summary(logit_ex3_1)

count<-c(36,8,33,23)
reg<-c(rep("y",2),rep("n",2)); reg<-as.factor(reg)
wed<-rep(c("w","s"),2); wed<-as.factor(wed)
reg; wed
log_ex1<-glm(count~wed+reg, family = poisson)
summary(logit_ex4)
anova(logit_ex4, test="Chisq")
log_ex1_1<-glm(count~wed*reg, family = poisson)
summary(logit_ex4_1)

count<-c(22, 16, 19, 11, 2, 54, 33, 17, 10, 115, 73,28)
loc<-rep(c("HN","T","E"), each=4)
type<-rep(c("H","S","N","I"), 3)
loc<-as.factor(loc); type<-as.factor(type)
log_ex2<-glm(count~type*loc, family = poisson)
log_ex2_1<-glm(count~type+loc, family = poisson)
summary(log_ex2)
anova(log_ex2_1, log_ex2, test="Chisq")
anova(log_ex2, test="Chisq")

### CH 5 example
init<-c(272,220,330,268,286,268,265,268,286,224,232,244,293,218,303,243,204,196,251,181)/10
res<-c(326,366,377,310,338,317,307,304,352,291,289,302,350,270,364,305,246,234,303,218)/10
treat<-rep(1:5, each=4); treat<-as.factor(treat); treat<-relevel(treat, ref=5)
anc1<-aov(res~treat*init)
summary(anc1)

anc1_1<-aov(res~treat+init)
summary(anc1_1)
anc1_2<-lm(res~treat+init)
summary(anc1_2)
# type 3 sum of squares
library(car)
Anova(anc1_2, type=3)

score<-c(37,37,45,41,57,49,49,53,53,53,61,49,53,57,49,53,53,53,57,49,53,53,53,61,45,53,57,
         49,53,53,53,53,53,61,49,37)
time<-c(61,37,53,41,41,33,49,53,45,53,37,65,45,57,49,53,45,53,57,49,53,45,53,45,53,37,45,
        45,57,49,53,45,53,37,65,37)
res<-c(11.3208,12.9151,18.8947,14.6739,8.6493,9.5238,7.6923,0.0017,8.0477,6.7358,6.1441,
       21.7939,4.2553,0.0017,11.0196,6.2762,13.2316,5.0676,5.6235,14.9893,13.7233,
       6.0669,8.1602,1.4423,6.9971,5.2308,8.256,14.5,20.7627,3.6115,11.3475,9.465,
       22.6103,0.002,20.5997,28.1828)
type<-rep(1:6, 6)
type<-as.factor(type); type<-relevel(type, ref=6)
anc2<-aov(res~type+score+time)
summary(anc2)
Anova(anc2, type=3)

anc2_2<-lm(res~type+score+time)
summary(anc2_2)

con<-c(40,54,85,95,81,26,90,95,83,83,85,83,65,98,47,74,75,97,79,91,65,25,34,20,30,29,100,85,24,26)
ox<-c(165,85,9,43,94,226,7,9,12,145,11,6,51,18,189,90,10,12,35,27,89,64,87,45,56,87,59,39,87,67)
treat<-rep(1:3, each=10)
treat<-relevel(as.factor(treat), ref=3)

anc3_1<-aov(ox~treat*con)
anc3_2<-aov(ox~con+treat)
anova(anc3_2, anc3_1, test="Chisq")
Anova(anc3_1, type=3)

anc3_3<-lm(ox~treat*con)
summary(anc3_3)

### CH 6 example
install.packages("psych")
library(psych)

pr1<-c(95,88,88,100,95,90,98,90,91,114,98,98,110,81,86,100,98,98,100,92,86,100,79,82,
       107,81,85,101,85,85,100,79,80,110,90,88,97,87,87,103,81,89,98,96,97,104,85,93,111,107,98,
       114,90,98,98,97,90,105,81,81,100,80,80,102,96,98,100,91,97,99,83,74,106,90,99,112,91,93,98,81,80,
       98,89,93,102,81,80,103,87,90,110,99,101,110,95,100)
pr2<-c(100,85,90,105,90,90,98,100,100,105,92,105,108,93,89,98,80,80,107,100,100,111,90,88,
       100,88,88,97,81,93,101,80,83,106,83,87,109,81,89,100,86,86,100,93,87,97,71,77,98,104,101,
       106,104,104,97,85,91,99,103,90,107,120,120,108,97,97,103,90,100,100,108,110,99,98,91,
       100,93,96,100,90,90,101,84,101,100,97,90,100,90,94,96,91,93,100,80,80,109,91,97,101,100,109)
subject1<-rep(paste("C",1:32), each=3); subject2<-rep(paste("E",1:34), each=3)
subject<-c(subject1, subject2)
treat<-c(rep("C",3*32), rep("E",34*3))
time<-rep(c(0,4,8),66)
time<-as.factor(time)
treat<-as.factor(treat)
pr<-c(pr1, pr2)

data<-data.frame(pr, time, treat, subject)
data$time<-as.factor(data$time)
data$treat<-as.factor(data$treat)
data$subject<-as.factor(data$subject)

raov1<-aov(pr~time*treat+Error(subject/(time*treat)), data=data)
summary(raov1, multivariate=F, type="III")

pr_mat<-matrix(pr, 66,3,byrow=T)
treat<-c(rep("C",32), rep("E",34)); treat<-as.factor(treat)
model<-lm(pr_mat~treat)
design<-as.factor(c("Week0","Week4","Week8"))

options(contrasts=c("contr.sum","contr.poly"))
results<-Anova(model, idata=data.frame(design), idesign = ~design, type="III")
summary(results, multivariate=T)

## Ch 7 example

#### non parametric method 
install.packages(c("survival","survminer"))
library("survival")
library("survminer")

data("lung")
head(lung)

?survfit
surv_fit<-survfit(Surv(time, status)~sex, data=lung)
summary(surv_fit)

# survival plot 
ggsurvplot(surv_fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

# cumulative hazard plot
ggsurvplot(surv_fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")

# log rank test
surv_diff <- survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff

head(lung)

time<-c(rep(0,456), rep(1,226+39), rep(2,152+22), rep(3, 171+23), rep(4, 135+24),
        rep(5,125+107), rep(6,83+133), rep(7, 74+102), rep(8, 51+68), rep(9,42+64), 
        rep(10, 43+45), rep(11,34+53), rep(12,18+33), rep(13, 9+27), rep(14, 6+23), rep(15,0+30))
status<-c(rep(2, 456), rep(2,226), rep(1,39), rep(2, 152), rep(1, 22), rep(2, 171), rep(1, 23),
          rep(2, 135), rep(1, 24),rep(2, 125), rep(1, 107),rep(2, 83), rep(1, 133),
          rep(2, 74), rep(1, 102),rep(2, 51), rep(1, 68),rep(2, 42), rep(1, 64),rep(2, 43), rep(1, 45),
          rep(2, 34), rep(1, 53),rep(2, 18), rep(1, 33),rep(2, 9), rep(1, 27),rep(2, 6), rep(1, 23),
           rep(1, 30))
dat<-data.frame(time, status)
surv_fit1<-survfit(Surv(time, status)~1, data=dat)
summary(surv_fit1)


ggsurvplot(surv_fit1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

time<-c(3,4,4.5,4.5,5.5,6,6.4,6.5,7,7.5,8.4,10,10,12,15)
status<-c(2,1,2,2,2,2,2,2,2,2,1,2,1,2,2)
dat<-data.frame(time, status)

surv_fit2<-survfit(Surv(time, status)~1, data=dat)
ggsurvplot(surv_fit2,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

time1<-c(33.7, 3.9,10.5,5.4,19.5,23.8,7.9,16.9,16.6,33.7,17.1)
stat1<-c(1,2,2,2,2,1,2,1,1,1,1)
time2<-c(8, 26.9,21.4,18.1,16,6.9,11,24.8,23,8.3,10.8,12.2,12.5,24.4,
         7.7,14.8,8.2,8.2,7.8)
stat2<-c(2,1,1,1,1,2,1,1,1,2,1,1,1,2,2,1,1,1,1)
treat<-c(rep(1,11), rep(2,19)); treat<-as.factor(treat)
time<-c(time1, time2); stat<-c(stat1, stat2)
dat<-data.frame(time, stat, treat)
surv_fit3<-survfit(Surv(time, stat)~treat, data=dat)
summary(surv_fit3)

ggsurvplot(surv_fit3,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

survdiff3<-survdiff(Surv(time, stat)~treat, data=dat)
survdiff3

#### parametric method
time2<-c(411,126,118,82,8,25,11,54,153,16,56,21,287,10,8,12,177,12,200,250,100,
         999,231,991,1,201,44,15,103,2,20,51,18,90,84,164,19,43,340,231)
stat2<-c(7,6,7,4,4,7,7,8,6,3,8,4,6,4,2,5,5,4,8,7,6,9,5,7,2,8,6,4,7,4,3,3,4,6,8,7,3,6,8,7)*10
age<-c(64,63,65,69,63,48,48,63,63,53,43,55,66,67,61,63,66,68,41,53,37,
       54,52,50,65,52,70,40,36,44,54,59,69,50,62,68,39,49,64,67)
post<-c(5,9,11,10,58,9,11,4,14,4,12,2,25,23,19,4,16,12,12,8,13,12,8,7,21,28,13,13,22,36,9,87
        ,5,22,4,15,4,11,10,18)
type<-c(rep(1,7),rep(2,7),3,3,4,4,4,4,4,rep(1,7),2,2,2,2,3,3,3,4,4,4,4,4)
cen<-rep(2,40); cen[c(6,23,29)]<-1
treat<-c(rep(1,21), rep(0,19))
type<-strata(type)
type

dat<-data.frame(time2, stat2,age, post, type, cen, treat)

surv_reg1<-survreg(Surv(time2, cen)~stat2+age+post+type+treat, data=dat,dist="weibull")
summary(surv_reg1)

### Cox regression
event<-c(12,10,7,10,6,8,8,9,11,13,7,13,9,12,13,8,10,16,6,14,13,13,16,13,9,
         9,10,12,7,7,7,7,11,16,16,6,15,9,10,17,8,8,8,8,14,13,9,15)
time<-c(8,12,52,28,44,14,3,52,35,6,12,7,52,52,36,52,9,11,52,15,13,21,24,52,28,
        15,44,2,8,12,52,21,19,6,10,15,4,9,27,1,12,20,32,15,5,35,28,6)
stat<-rep(2,48); stat[c(2,3,8,12:14,16,18,19,23,24,27:28,31,36,38,43)]<-1
treat<-c(rep(1,25), rep(2,23)); treat<-as.factor(treat)

dat2<-data.frame(event, time, stat, treat)

cox1<-coxph(Surv(time, stat)~event+treat, data=dat2)
summary(cox1)

cox_fit1<-survfit(cox1)
plot(cox_fit)

type
cox_fit2<-coxph(Surv(time2, cen)~stat2+age+post+treat+type, data=dat)
summary(cox_fit2)

cox2<-survfit(cox_fit2)
plot(cox2)

### Ch8
install.packages("metap")
library(metap)
wilkinsonp()

p<-c(0.0029,0.051,.631,.3783,.0034,.0305,.0341,.0367,.573,.3517)

# order statistic method
minimump(p)

# inverse chi square method
?invchisq
invchisq(p, k=2)

# inverse normal method?

# inverse logistic method
metap::logitp(p)

# homogeneous p-values?
z<-qnorm(p)
test.stat<-sum((z-mean(z))^2)
1-pchisq(test.stat, df=9)

homo_test<-function(p, k){
  z<-qnorm(p)
  test.stat<-sum((z-mean(z))^2)
  p_val<-1-pchisq(test.stat, df=k-1)
  return(list(Test_statistics=test.stat, p_value=p_val))
}

homo_test(p,10)
library(meta)
# estimation of effect size
qnorm(0.05)
effect_size_CI<-function(n1,n2,s1,s2,m1,m2,alpha){
  S2<-((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)
  S<-sqrt(S2)
  delta1<-(m1-m2)/S
  delta2<-4*(n1+n2-3)/(4*(n1+n2)-9)*delta1
  S2_hat<-(n1+n2)/(n1*n2)+delta2^2/(2*(n1+n2))
  S_hat<-sqrt(S2_hat)
  Upper<-delta2-qnorm(alpha/2)*S_hat
  Lower<-delta2+qnorm(alpha/2)*S_hat
  return(list(estimate=delta2, Lower=Lower,Upper=Upper))
}

effect_size_CI(10,10,5,3,65,62.53,.05)
?sinh
# using arc hyperbolic sign
vst_CI1<-function(n1, n2, d, alpha){
  a<-sqrt(4+2*(n1/n2)+2*(n2/n1))
  hd<-sqrt(2)*asinh(d/a)
  tu<-hd-qnorm(alpha/2)/sqrt(n1+n2)
  tl<-hd+qnorm(alpha/2)/sqrt(n1+n2)
  u<-a*sinh(tu/sqrt(2)); l<-a*sinh(tl/sqrt(2))
  return(list(Lower=l, Upper=u))
}

vst_CI1(10,10,0.5737505,.05)

# using correlation coefficient
vst_CI2<-function(n1,n2,d,alpha){
  v<-(n1+n2)*(n1+n2-2)/(n1*n2)
  r<-d/sqrt(d^2+v)
  cv<-qt(alpha/2, df=(n1+n2-2))
  ul<-cv/sqrt(n1+n2-2+cv^2); uu<--cv/sqrt(n1+n2-2+cv^2)
  pl<-(uu-r)/(r*uu-1); pu<-(ul-r)/(r*ul-1)
  l<-pl*sqrt(v)/sqrt(1-pl^2); u<-pu*sqrt(v)/sqrt(1-pu^2)
  return(list(Lower=l, Upper=u))
}

vst_CI2(10,10,0.57,0.05)

ne<-c(100,131,40,40,97,28,60,72,87,80,79,40,36,9,14,21,133,82)
nc<-c(180,138,40,40,47,61,55,102,45,49,55,109,93,18,16,22,124,45)
d<-c(.1,-.162,-.09,-.049,-.046,-.01,-.431,-.261,.134,.019,.175,.056,.045,.103,.121,-.482,-.29,.342)
sig<-c(156,149,501,500,316,521,357,239,338,329,309,342,385,1669,1342,958,157,347)/1e+4
sig
mat<-cbind(ne,nc,d,sig)
weighted_linear_comb<-function(mat, alpha){
  dp<-sum(mat[,3]/mat[,4])/sum(1/mat[,4])
  sig_dp<-sqrt(1/sum(1/mat[,4]))
  u<-dp-qnorm(alpha/2)*sig_dp
  l<-dp+qnorm(alpha/2)*sig_dp
  return(list(estimate=dp, lower=l, upper=u))
}
weighted_linear_comb(mat, 0.05)

vst_comb1<-function(n, eff_size, alpha){
  N<-sum(n)*2
  hd<-sqrt(2)*asinh(eff_size/(2*sqrt(2)))
  hp<-sum(2*n*hd)/N
  cv<-qnorm(alpha/2)
  hl<-hp+cv/sqrt(N); hu<-hp-cv/sqrt(N)
  l<-2*sqrt(2)*sinh(hl/sqrt(2)); u<-2*sqrt(2)*sinh(hu/sqrt(2))
  return(list(Lower=l, Upper<-u))
}

n<-c(22,10,10,10,39,50)
d<-c(0.563, 0.308, 0.081, 0.598,-0.178,-0.234)
vst_comb1(n,d,0.05)

vst_comb2<-function(n, eff_size, alpha){
  N<-sum(n)*2
  r<-eff_size/sqrt(eff_size+4)
  z<-log((1+r)/(1-r))/2
  zp<-sum((2*n-3)*z)/(N-3*length(n))
  cv<-qnorm(alpha/2)
  el<-zp+cv/sqrt(N-3*length(n)); eu<-zp-cv/sqrt(N-3*length(n))
  pl<-(exp(2*el)-1)/(exp(2*el)+1); pu<-(exp(2*eu)-1)/(exp(2*eu)+1)
  l<-2*pl/sqrt(1-pl^2); u<-2*pu/sqrt(1-pu^2)
  return(list(Lower=l, Upper=u))
}

vst_comb2(n,d,.05)

# large sample
homo_size_test<-function(mat, alpha){
  dp<-weighted_linear_comb(mat, alpha)$estimate
  stat<-sum((mat[,3]-dp)^2/mat[,4])
  p<-1-pchisq(stat, df=dim(mat)[1]-1)
  return(list(stat=stat, p_value=p))
}
homo_size_test(mat, 0.05)

vst_homosize_test<-function(n, eff_size){
  N<-sum(n)*2
  hd<-sqrt(2)*asinh(eff_size/(2*sqrt(2)))
  hp<-sum(2*n*hd)/N
  Q<-2*sum(n*hd^2)-N*hp^2
  p<-1-pchisq(Q, df=(length(n)-1))
  return(list(test_stat=Q, p_value=p))
}

vst_homosize_test(n,d)

vst2_homosize_test<-function(n, eff_size){
  N<-sum(n)*2
  r<-eff_size/sqrt(eff_size+4)
  z<-log((1+r)/(1-r))/2
  zp<-sum((2*n-3)*z)/(N-3*length(n))
  Q<-sum((2*n-3)*(z-zp)^2)
  p<-1-pchisq(Q, df=(length(n)-1))
  return(list(test_stat=Q, p_value=p))
}

vst2_homosize_test(n, d)
