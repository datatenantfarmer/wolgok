setwd("/Users/chanheelee/Desktop/??????/CDA/raw data")
# cda hw 3
# a) interpret fit
snore<-read.table("Snoring.txt", header=T)
snore
dim(snoring)
snore$scores<- c(0,2,4,5)
model1<-glm(cbind(snore$yes, snore$no)~scores, family=binomial(link="identity"), data=snore)
summary(model1)
# binary regression under consideration of overdispersion
model1_1<-glm(cbind(snore$yes, snore$no)~scores, family=quasibinomial(link="identity"), data=snore)
summary(model1_1)
model2<-glm(cbind(snore$yes, snore$no)~snoring, family=binomial, data=snore)
summary(model1)
model3<-glm(cbind(snore$yes, snore$no)~snoring, family=binomial(link="probit"), data=snore)
summary(model3)

# b) interpret fit + overdispersion
crab<-read.table("Crabs.txt", header=T)
model4<-glm(sat~width, family = poisson, data=crab)
summary(model4)

model4_1<-glm(sat~width, family=quasipoisson(link="log"), data=crab)
summary(model4_1)

model4_2<-glm.nb(sat~width, data=crab)
summary(model4_2)
# c) interpret fit + overdispersion
train <- data.frame(
  matrix(c(2003, 518,0,3,	1988, 443,2,4,
           2002, 516,1,3,	1987, 397,1,6,
           2001, 508,0,4,	1986, 414,2,13,
           2000, 503,1,3,	1985, 418,0,5,
           1999, 505,1,2,	1984, 389,5,3,
           1998, 487,0,4,	1983, 401,2,7,
           1997, 463,1,1,	1982, 372,2,3,
           1996, 437,2,2,	1981, 417,2,2,
           1995, 423,1,2,	1980, 430,2,2,
           1994, 415,2,4,	1979, 426,3,3,
           1993, 425,0,4,	1978, 430,2,4,
           1992, 430,1,4,	1977, 425,1,8,
           1991, 439,2,6,	1976, 426,2,12,
           1990, 431,1,2,	1975, 436,5,2,
           1989, 436,4,4), ncol=4, byrow=T))
names(train) <- c("year", "trainkm", "traincol", "trainroad")

train$year<-train$year-min(train$year)
train
model5<-glm(trainroad~year, family=poisson, offset = log(trainkm), data=train)
summary(model5)

library("MASS")
model5_1<-glm.nb(trainroad~year, offset(log(trainkm)), data=train)
summary(model5_1)
# d) interpret fit
model6<-glm(y~width, family = binomial, data=crab)
summary(model6)
# e) interpret fit + combine some color categories
crab$F_C<-relevel(factor(crab$color), ref=4)
model7<-glm(y~width+F_C, family = binomial, data=crab)
summary(model7)
# f) conditional or and model comparison using deviance
aid<-read.table("Aids.txt", header = T)
model8<-glm(cbind(aid$yes, aid$no)~race+azt, family=binomial, data=aid)
summary(model8)
# g) describning group effect + CMH test
clinic<-read.table("Infection.txt", header=T)
clinic$no<-clinic$n-clinic$y
clinic$yes<-clinic$y
table = c(11,10,25,27,16,22,4,10,14,7,5,12,2,1,14,16,6,0,11,12,1,0,10,10,1,1,4,8,4,6,2,1)
table = array(beitler, dim=c(2,2,8))
dimnames(table) = list("Treatment" = c("Drug", "Control"),
                         "Response" = c("Success", "Failure"), "Center" = c(1:8))
mantelhaen.test(table)
clinic1<-read.table("Infection.txt", header=T)
clinic$F_C<-relevel(factor(clinic$center), ref=8)
model9<-glm(cbind(clinic$yes, clinic$no)~F_C+treat, family = binomial, data=clinic)
summary(model9)
# h) model checking + checking
crab$F_S<-relevel(factor(crab$spine), ref=3)
model10<-glm(y~F_C*F_S+F_C*width+F_S*width, family = binomial, data=crab)
summary(model10)
model10_1<-glm(y~F_C+F_S+width, family=binomial, data=crab)
summary(model10_1)
model10_2<-glm(y~F_C+F_S, family = binomial, data=crab)
model10_3<-glm(y~F_C+width, family = binomial, data=crab)
model10_4<-glm(y~F_S+width, family=binomial, data=crab)
model10_5<-glm(y~F_C, family = binomial, data=crab)
model10_6<-glm(y~width, family=binomial, data=crab)
crab$color2<-ifelse(crab$color==4, 0, 1)
model10_7<-glm(y~width+color2, family=binomial, data=crab)
model10_8<-glm(y~1, family = binomial, data=crab)
anova(model10_6, model10_7, test="Chisq")

# i) model checking - dfbeta, sresid, c, pearson difference, LR difference, ROC curve
heart <- data.frame( list( pressure=c(111.5,121.5,131.5,141.5,151.5,161.5,176.5,191.5),
                           sample.size=c(156,252,284,271,139,85,99,43),
                           observed=c(3,17,12,16,12,8,16,8) ) )
heart
heart
model11<-glm(observed/sample.size~pressure, family = binomial, weights = sample.size, data=heart)
summary(model11)
# j) classification table
model12 = glm(y ~ width + F_C, family=binomial, data=crab)
# use stepwise method for selecting feature
model12_1 = glm(y ~ F_C+width+F_S+weight, family=binomial, data=crab)
# Use user - defined function to construct classification table

# 2
age<-rep(1:6, 2)
region<-c(rep("south", 6), rep("north", 6))
cases<-c(64, 75, 68,63, 45, 27, 61, 76,98, 104,63, 80)          
total = c(1074246, 220407, 198119, 134084, 70708, 34233, 2880262, 564535, 592983, 450740, 270908, 161850)
# a)
prob2<-data.frame(age, region, cases, total)
prob2$age<-relevel(factor(prob2$age), ref=1)
prob2$region<-relevel(factor(prob2$region), ref="south")
# poisson regression
model13<-glm(cases~age+region, family = poisson, data=prob2)
summary(model13)
model13_1<-glm(cases~age+region, family = poisson, offset = log(total), data=prob2)
model13_2<-glm(cases~age*region, family = poisson, offset = log(total), data=prob2)
anova(model13_1, model13_2, test="Chisq")
summary(model13_1)
# b)
# negative binomial regression
model14<-glm.nb(cases~age+region, data=prob2)
summary(model14)
model14_1<-glm.nb(cases~age+region, offset(log(total)), data=prob2)
summary(model14_1)

# c) compare a) and b) + interpret final model

# 3
disease = c(4,8,9,21)
n = c(15,18,18,27)
gender<-c(rep("Female",2), rep("Male",2))
no_disease<-n-disease
ECG<-rep(c("<0.1", ">0.1"),2)
response = data.frame(gender, ECG, disease, no_disease)
response

model15<-glm(cbind(response[,3], response[,4])~ECG, family=binomial, data=response)
model15_1<-glm(cbind(response[,3], response[,4])~1, family=binomial, data=response)
anova(model15_1, model15, test="Chisq")
summary(model15)

model16<-glm(cbind(response[,3], response[,4])~ECG+gender, family=binomial, data=response)
summary(model16)
model16_1<-glm(cbind(response[,3], response[,4])~ECG*gender, family=binomial, data=response)
summary(model16_1)
anova(model16, model16_1, test="Chisq")
# 4
summary(model10_1)
val<-sum((crab$y-fitted(model17_2))^2/fitted(model17_2))

1-pchisq(val, df=length(crab$y)-3)
df=length(crab$y)-3
df
inf<-influence.measures(model17_2)
inf
# 19, 25, 26, 29, 60, 82, 86, 88, 93, 117, 134, 155, 164, 172
sum((crab[-19,]$y-fitted(glm(y~color2+width, family=binomial, data=crab[-19,]))^2/fitted(model17_2))

model17_2<-glm(y~color2+width, family=binomial, data=crab)
summary(model17_2)
summary(glm(y~color2+width, family=binomial, data=crab[-172,]))
install.packages("pROC")
library(ROCR)
fitted(model17_2)

pred <- prediction(fitted(model17_2), crab$y)  ; perf <- performance(pred, "tpr", "fpr")
pred
plot(perf,xlab = "1-specificity", ylab = "sensitivity", main = "ROC Curve")    ##ROC Curve
performance(pred,"auc")@y.values
exp(0.4643)
0.2857143*1.6

1/exp(0.7885+0.4643) 
1/exp(0.77885) 
influence.measures(model10_7)
# 19, 25, 26, 29, 60, 82, 86, 88, 93, 117, 134, 155, 164, 172

model10_7<-glm(y~width+color2, family=binomial, data=crab)
summary(model10_7)
install.packages("generalhoslem")
library(ResourceSelection)
library(generalhoslem)
hoslem.test(model10_7$y, fitted(model10_7))
inf<-as.data.frame(influence.measures(model10_7))
a<-influence.measures(model10_7)
a<-as.list(a)
dim(a)
fitted(model10_7)
r.st<-rstandard(model10_7)
plot(r.st)

summary(model10_7)
le<-hatvalues(model10_7)

plot(crab$y~crab$width)
plot(crab$y~crab$color2)
