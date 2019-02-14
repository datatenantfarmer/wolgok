### Non - linearity modeling ###

library(ISLR)
wage<-Wage
wage
agelims=range(age)
age.grid<-seq(agelims[1], agelims[2])

### polynomial regression and step functions ###
model.poly<-lm(wage~poly(age, 4), data=wage) # poly(variable, degree of ploynomial)
summary(model.poly)
pred<-predict(model.poly, newdata=list(age=age.grid), se=T)
se.bands<-cbind(pred$fit+2*pred$se.fit, pred$fit-2*pred$se.fit)
dim(se.bands)
length(age.grid)
plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(age.grid, pred$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands, lwd=1, col="red", lty=3, cex=8)
model.poly2<-lm(wage~poly(age, 4, raw=T), data=wage)
summary(model.poly2)


# Selecting degree of polynomials
fit1<-lm(wage~poly(age, 1), data=wage)
fit2<-lm(wage~poly(age, 2), data=wage)
fit3<-lm(wage~poly(age, 3), data=wage)
fit4<-lm(wage~poly(age, 4), data=wage)
fit5<-lm(wage~poly(age, 5), data=wage)
anova(fit1, fit5)
anova(fit2, fit5)
anova(fit3, fit5)
anova(fit4, fit5)

fit6<-lm(wage~poly(age, 1)+education, data=wage)
fit7<-lm(wage~poly(age, 2)+education, data=wage)
fit8<-lm(wage~poly(age, 3)+education, data=wage)
anova(fit6, fit8)
anova(fit7, fit8)

# logistic regression
# I() : create dummy variables
model.log.poly<-glm(I(wage>250)~poly(age,4), data=wage, family = binomial)
summary(model.log.poly)

# Fit Step function
table(cut(wage$age, 4))
model.step<-lm(wage~cut(age, 4), data=wage)
summary(model.step)
pred<-predict(model.step, newdata=list(age=age.grid), se=T)
se.bands<-cbind(pred$fit+2*pred$se.fit, pred$fit-2*pred$se.fit)
plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(age.grid, pred$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands, lwd=1, col="red", lty=3, cex=8)

### Splines ###
library(splines)
# bs() : generate basis functions for splines with specified # of knots
# default : Cubic splines
bs(wage$age, knots=c(25, 40, 60))
model.spline<-lm(wage~bs(age, knots=c(25, 40, 60)), data=wage)
summary(model.spline)
pred<-predict(model.spline, newdata=list(age=age.grid), se=T)
se.bands<-cbind(pred$fit+2*pred$se.fit, pred$fit-2*pred$se.fit)
plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(age.grid, pred$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands, lwd=1, col="red", lty=3, cex=8)
# for the knot outside, CI is relatively wide -> 'natuaral spline' 

# 1: 1st degree polynomial 
# 2: 2nd degree polynomial
# 3: 3rd degree polunomial
# 4: 1st truncated power basis function
# 5: 2nd truncated power basis function
# 6: 3rd truncated power basis function

# natural splines : smaller than smallest knot & larger then largest knot -> linear instead of cubic
model.nspline<-lm(wage~ns(age, df=4), data=wage)
summary(model.nspline)

pred<-predict(model.nspline, newdata=list(age=age.grid), se=T)
se.bands<-cbind(pred$fit+2*pred$se.fit, pred$fit-2*pred$se.fit)
plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(age.grid, pred$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands, lwd=1, col="red", lty=3, cex=8)

plot(wage$wage~wage$age, col="grey", xlim=agelims, main="Cubic spline Vs Natural spline")
pred.nat<-predict(model.nspline, newdata=list(age=age.grid), se=T)
se.bands.nat<-cbind(pred.nat$fit+2*pred.nat$se.fit, pred.nat$fit-2*pred.nat$se.fit)
lines(age.grid, pred.nat$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands.nat, lwd=2, col="orange", lty=3, cex=8)
pred.cu<-predict(model.spline, newdata=list(age=age.grid), se=T)
se.bands.cu<-cbind(pred.cu$fit+2*pred.cu$se.fit, pred.cu$fit-2*pred.cu$se.fit)
lines(age.grid, pred.cu$fit, col="red", lwd=2, cex=8)
matlines(age.grid, se.bands.cu, lwd=2, col="red", lty=3, cex=8)
legend("topleft", legend=c("Natural spline", "Cubic spline"), col=c("orange", "red"), lty=c(1,1))


plot(wage$wage~wage$age, col="grey", xlim=agelims, main="Polynomial regression Vs Natural spline")
pred.nat<-predict(model.nspline, newdata=list(age=age.grid), se=T)
se.bands.nat<-cbind(pred.nat$fit+2*pred.nat$se.fit, pred.nat$fit-2*pred.nat$se.fit)
lines(age.grid, pred.nat$fit, col="orange", lwd=2, cex=8)
matlines(age.grid, se.bands.nat, lwd=2, col="orange", lty=3, cex=8)

model.poly3<-lm(wage~poly(age, 15), data=wage)
pred.pol<-predict(model.poly3, newdata=list(age=age.grid), se=T)
se.bands.pol<-cbind(pred.pol$fit+2*pred.pol$se.fit, pred.pol$fit-2*pred.pol$se.fit)
lines(age.grid, pred.pol$fit, col="red", lwd=2, cex=8)
matlines(age.grid, se.bands.pol, lwd=2, col="red", lty=3, cex=8)
legend("topleft", legend=c("Natural spline", "Polynomial regression"), col=c("orange", "red"), lty=c(1,1))


# smoothing spline
# Effective df : in fact, n df. But, parameter is shrinked by tuning parameter
# for same df, flexibility differs by value of tuning parameter 
model.ss<-smooth.spline(wage$age, wage$wage, df=16)
model.ss$fit
plot(wage$wage~wage$age, col="grey")
points(fitted(model.ss)~wage$age, col="red")
model.ss$df

model.ss2<-smooth.spline(wage$age, wage$wage, cv=TRUE)
model.ss2$df
points(model.ss2, col="blue")
model.ss$fit
model.ss2$fit

plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(model.ss, col="orange", lwd=2, cex=8)
lines(model.ss2, col="red", lwd=2)
legend("topleft", legend=c("16 df", "6.8 df by cv"), col=c("orange","red"), lty=c(1,1))
# small df indicate smoother fit

# Local regression
model.local1<-loess(wage~age, span=.2, data=wage)
model.local2<-loess(wage~age, span=.5, data=wage)
plot(wage$wage~wage$age, col="grey", xlim=agelims)
lines(age.grid, predict(model.local1, data.frame(age=age.grid)), col="orange", lwd=2, cex=8)
lines(age.grid, predict(model.local2, data.frame(age=age.grid)), col="red", lwd=2, cex=8)


### GAMs ###
gam1<-lm(wage~ns(year, 4)+ns(age, 5)+education, data=wage)
summary(gam1)
plot(gam1)

install.packages("gam")
library(gam)
# s() : smoothing spline
gam2<-gam(wage~s(year,4)+s(age, 5)+education, data=wage)
summary(gam2)
plot(gam2, se=T)

# local regression fit
gam.lo<-gam(wage~s(year,4)+lo(age, span=0.7)+education, data=wage)
summary(gam.lo)
plot(wage$wage~wage$age, col="grey")
points(fitted(gam.lo)~wage$age, col="red")
