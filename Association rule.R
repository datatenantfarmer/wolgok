# Algorithm 1 : Association rule
library(arules)
library(arulesViz)
grocery<-read.csv("Grocery.csv")
grocery

# load data as transaction data
Grocery = read.transactions("Grocery.csv", format = "single", cols = c(1,3), sep=",", skip=1, rm.duplicate=TRUE)
inspect(Grocery)

str(Grocery)
as(Grocery, "data.frame")[1:10,]

# Generate association rules
?apriori
rules = apriori(Grocery, parameter=list(support=0.1, confidence=0.7, minlen=2),
                control=list(verbose=F))
rules.sorted = sort(rules, by=c("support","lift")) ##sorting data
inspect(rules.sorted)

# Generate subset of rules
rules.sub = subset(rules, subset = rhs %in% "heineken" & lift > 1) 
inspect(rules.sub)

# plotting association rule
plot(rules, method="graph", engine = 'interactive')
plot(rules, method="grouped", measure=c("lift","support"))


# HW1 Association rules
bank<-read.transactions("BNKSERV.csv", format="single",cols=c(1,2), sep=",",skip=1, rm.duplicates = T)
inspect(bank)

as(bank, "data.frame")[1:10,]

rules_bank<-apriori(bank, parameter=list(support=0.1, confidence=0.7,minlen=2),
                    control=list(verbose=F))
rules_bank_sorted<-sort(rules_bank, by=c("confidence","lift"))
inspect(rules_bank_sorted)

plot(rules_bank, method="grouped", measure=c("lift","confidence"))
# Those who certificate of deposit only don't check his account
# Those who certificate of deposit and saving account check his account
rules.sub<-subset(rules_bank, subset=lhs %in% "CD" & lift>1)
inspect(rules.sub)

rules.sub<-subset(rules_bank, subset=lhs %in% "CD" & lift<1)
inspect(rules.sub)

# Those who using ATM check his account
rules.sub<-subset(rules_bank, subset=lhs %in% "ATM" & lift>1)
inspect(rules.sub)

# Those who receive credit card service seems to check his account
rules.sub<-subset(rules_bank, subset=lhs %in% "CKCRD" & lift>1)
inspect(rules.sub)

rules.sub<-subset(rules_bank, subset=lhs %in% "CKCRD" & lift<1)
inspect(rules.sub)


rules.sub<-subset(rules_bank, subset=lhs %in% "" & lift<1)
inspect(rules.sub)
