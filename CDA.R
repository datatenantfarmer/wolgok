setwd("/Users/chanheelee/Desktop/??????/CDA/raw data")
crab<-read.table("Crabs.txt", header=T)
crab
plot(crab$y~crab$width)
model1<-glm(y~width, data=crab, family = binomial)
summary(model1)
fit<-fitted(model1)
plot(fit~crab$width)
plot(y~width, data=crab)
cor(crab$y, fitted(model1))
# median effective level
-coef(model1)[1]/coef(model1)[2]

model1_1<-glm(y~1, data=crab, family = binomial)
anova(model1_1, model1, test="Chisq")

aid<-read.table("Aids.txt", header = T)
aid
model2<-glm(cbind(aid[,3], aid[,4])~race+azt, data=aid, family = binomial)
summary(model2)
exp(-0.72)

model2_1<-glm(cbind(aid[,3], aid[,4])~race, data=aid, family = binomial)
anova(model2_1, model2, test="Chisq")

crab$F_C<-relevel(factor(crab$color), ref="4")
crab
model3<-glm(y~width+F_C, data=crab, family = binomial)
summary(model3)
anova(model1, model3, test="Chisq")
hoslem.test(crab$y, fitted(model3))
hoslem.test(crab$y, fitted(model1))

model3_1<-glm(y~width+color, data=crab, family = binomial)
summary(model3_1)
exp(-0.509)

anova(model3_1, model3, test="Chisq")

crab$F_C2<-ifelse((crab$color==4),0,1)
crab

model3_2<-glm(y~width+F_C2, data=crab, family = binomial)
summary(model3_2)

anova(model3_2, model3, test="Chisq")

model4<-glm(y~width*F_C2, data=crab, family = binomial)
summary(model4)

anova(model3_2, model4, test="Chisq")

# Baseline category logit model
# Alligator's food choice
library(nnet)
alli<-read.table("Alligators.txt", header=T)
alli
alli$y<-relevel(factor(alli$y), ref="O")
model4<-multinom(y~x, data=alli)
summary(model4)
model4_1<-multinom(y~1, data=alli)
summary(multinom(y~1, data=alli))
model4_1$edf
model4$edf
(model4_1$deviance-model4$deviance)
1-pchisq((model4_1$deviance-model4$deviance), df = model4$edf-model4_1$edf)
summary(model4)
fit<-fitted(model4)
plot(fit[,1]~x, data=alli, type="l", ylim=c(0,1))
lines(fit[,2]~x, data=alli, col="red")
lines(fit[,3]~x, data=alli, col="blue")
# Belief in after life
race<-rep(c("White", "Black"), each=2)
gender<-rep(c("female", "male"), 2)
gender
yes<-c(371,250,64,25)
un<-c(49,45,9,5)
no<-c(74,71,15,13)
belief<-data.frame(race, gender, yes,un, no)
?multinom
model5<-multinom(cbind(yes,un, no)~gender+race, data=belief)
summary(model5)
model5_1<-multinom(cbind(yes,un, no)~race, data=belief)
1-pchisq(-model5$deviance+model5_1$deviance, df=2)
model5_2<-multinom(cbind(yes,un, no)~gender, data=belief)
fit2<-fitted(model5_2)
fit2
plot(fit2[,1]~gender, data=belief, type="l")
lines(fit2[,2]~gender, col="red")
library("VGAM")
income<-c(1,2,3)
not<-rep(6,3)
pretty<-c(43,113,57)
very<-c(75,178,117)

# Mariage Happiness and income
library(nnet)
mariage<-data.frame(income, not, pretty, very)

library(VGAM)
model1<-vglm(y~income, family = multinomial(refLevel = "very"), data=mariage)
summary(model1)
model1_1<-vglm(y~1, family = multinomial(refLevel = "very"), data=mariage)
summary(model1_1)
model3<-vglm(y~income, family = multinomial(refLevel = "not"), data=mariage)
summary(model3)
com<-deviance(model1_1)-deviance(model1)
1-pchisq(com, 2)
fitted(model1)

model2<-vglm(y~income, family = cumulative(parallel = T), data=mariage)
summary(model2)
model2_2<-vglm(y~income, family = cumulative(parallel = FALSE), data=mariage)
summary(model2_2)
1-pchisq(deviance(model2)-deviance(model2_2), df=1)
model2_1<-vglm(y~1, family = cumulative(parallel = T), data=mariage)
summary(model2_1)

1-pchisq(deviance(model2)-deviance(model2_1), 2)

idea<-read.table("idea.txt", header=T)
idea$party<-relevel(factor(idea$party), ref="Rep")
y<-as.matrix(idea[,3:7])
model4<-vglm(y~gender+party, family = cumulative(parallel = T), data=idea)
summary(model4)
model4_1<-vglm(y~party, family = cumulative(parallel = T), data=idea)
1-pchisq(deviance(model4_1)-deviance(model4), df=1)
summary(model4_1)
exp(0.9825)
fitted(model4_1)

model4_2<-vglm(y~party, family = cumulative(parallel = FALSE), data=idea)
summary(model4_2)


1-pchisq(deviance(model4_1)-deviance(model4_2), df=3)

mental<-read.table("Mental.txt", header=T)
mental
model5<-vglm(impair~ses+life, family = cumulative(parallel = T), data=mental)
summary(model5)
model5_1<-vglm(impair~ses+life, family = cumulative(parallel = FALSE), data=mental)
summary(model5_1)
1-pchisq(deviance(model5)-deviance(model5_1), df= 4)

# log - linear model
yes<-c(1339,260,88)
no<-c(1639-1339, 315-260, 110-88)
count<-c(yes,no)
res<-rep(c("yes", "no"), each = 3)
race<-rep(c("w", "b", "o"),2)
race
belief<-data.frame(race, res, count)
belief
belief$race<-relevel(factor(belief$race), ref="o")
model6<-glm(count~race+res, family = poisson, data=belief)
summary(model6)
model6_1<-glm(count~race*res, family = poisson, data=belief)
summary(model6_1)
anova(model6, model6_1, test="Chisq")

alcohol<-rep(c("yes", "no"), each=4)
cigar<-rep(rep(c("yes","no"), each=2), 2)
mari<-rep(c("yes","no"), 4)
cbind(alcohol, cigar, mari)
count<-c(911, 538,44,456,3,43,2,279)

ACM<-data.frame(alcohol, cigar, mari, count)
ACM
model7<-glm(count~alcohol+cigar+mari, family = poisson, data=ACM)
summary(model7)
model7_2<-glm(count~alcohol+cigar+mari+alcohol*cigar+cigar*mari+alcohol*mari, family = poisson, data=ACM)
summary(model7_2)
fitted(model7_2)
ACM$count
model7_3<-glm(count~alcohol+cigar+mari+alcohol*cigar+cigar*mari+alcohol*mari+alcohol*mari*cigar, family = poisson, data=ACM)
summary(model7_3)
anova(model7_2, model7_3, test="Chisq")

# chi - squared test
1-pchisq(sum((ACM$count-fitted(model7_2))^2/fitted(model7_2)), df=1)
a<-rstandard(model7_2)
a^2

model7_4<-glm(count~alcohol+cigar+mari+cigar*mari+alcohol*mari, family = poisson, data=ACM)
anova(model7_4, model7_2, test="Chisq")

# opnions 
G<-rep(c("male", "female"), each=4)
I<-rep(rep(c("support", "oposite"), each=2),2)
H<-rep(c("heath", "opinion"), 4)
count<-c(76, 160, 6, 25, 114, 181, 11, 48)

op<-data.frame(G, I, H, count)
op

model8<-glm(count~G+I+H+G*I+G*H+H*I, family = poisson, data=op)
ea<-(op$count-fitted(model8))^2/fitted(model8)
1-pchisq(sum(ea), df=1)
confint(model8)

exp(-0.0009176702); exp(0.9451922)
model8_1<-glm(count~G+I+H+G*H+H*I, family = poisson, data=op)
anova(model8_1, model8, test="Chisq")
qnorm(0.8, 0,1)
qnorm(.025, 0,1)

acc<-read.table("Accidents.txt", header=T)
colnames(acc)<-c("G","L","S","I", "count")
model9<-glm(count~G+L+S+I, family=poisson, data=acc)

model9_1<-glm(count~G+L+S+I+G*L+G*S+G*I+L*S+L*I+S*I, family=poisson, data=acc)
model9_2<-glm(count~G+L+S+I+G*L+G*S+G*I+L*S+L*I+S*I+G*I*L+G*I*S+G*L*S+I*L*S, family=poisson, data=acc)
model9_3<-glm(count~G+L+S+I+G*L+G*S+G*I+L*S+L*I+S*I+G*L*S, family=poisson, data=acc)
model9_S<-glm(count~G*L*S*I, family=poisson, data=acc)

# G2 
anova(model9, model9_S, test="Chisq")
anova(model9_1, model9_S, test="Chisq")
anova(model9_2, model9_S, test="Chisq")

summary(model9_1)
summary(model9_2)
OR_homo<-exp(coef(model9_1)[6:11])
OR_homo

summary(model9_3)
1/exp(-0.28274); 1/exp(-0.28274+0.12858)
exp(-0.540+.13); exp(-0.54)
1/exp(-.16+.13)
1/exp(-.16)
anova(model9_1, model9_3, test="Chisq")
anova(model9_3, model9_S, test="Chisq")
sum(acc$count)

# dissimilarity index : the proportion of sample should be removed to achieve perfect fit
dissimilar_index<-function(n,model,count){
  val<-abs(count-fitted(model))
  D<-sum(val)/(2*n)
  return(D)
}

dissimilar_index(sum(acc$count), model9_1, acc$count)
dissimilar_index(sum(acc$count), model9_3, acc$count)

M<-rep(c("B", "G"), 4)
S<-rep(rep(c("L","H"), each=2),2)
W<-rep(c("L","H"), 4)
count<-c(103, 87, 32, 42, 59, 109, 78, 205)

man<-data.frame(M,S,W, count)
man

model<-glm(count~M+S+W, family = poisson, data=man)
summary(model)

school<-rep(c(1,2,3), each=2)
style<-rep(c("r", "a"), 3)
self<-c(10,5,21,16,15,12)
team<-c(17,12,17,12,15,12)
class<-c(26,50,26,36,16,20)
pre<-data.frame(school, style, self, team, class)
library(VGAM)
pre$school<-relevel(factor(pre$school), ref=3)
pre$style<-relevel(factor(pre$style), ref="a")
model<-vglm(cbind(self, team, class)~school+style, family = multinomial(refLevel = "class"), data=pre)
summary(model)
fitted(model)


sample(c(1,0), 1, prob=c(0.5,0.5))
