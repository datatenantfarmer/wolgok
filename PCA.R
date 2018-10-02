### Swiss bank notes data ###
setwd("/Users/chanheelee/Desktop/lecture/Multivariate Data Analysis/raw data")

bank<-read.table("bank2.txt", header=T)
bank$status<-c(rep("Genuine", 100), rep("Counterfeit",100))
colnames(bank)

library(car)
scatterplotMatrix(~Length+Left+Right+Bottom+Top+Diagonal|status, data=bank , reg.line="" , smoother="", col=c("orange","grey") , smoother.args=list(col="grey") , cex=1 , pch=c(15,16))

pc_note<-prcomp(bank[,1:6], scale.=T)
pc_note$x
pc_score_note<-as.data.frame(pc_note$x)
pc_score_note$status<-bank$status
scatterplotMatrix(~PC1+PC2+PC3+PC4+PC5+PC6|status, data=pc_score_note , reg.line="" , smoother="", col=c("orange","grey") , smoother.args=list(col="grey") , cex=1 , pch=c(15,16))
# Screeplot
screeplot(pc_note, type="l")
pc_note$sdev^2
# retained PCs
scatterplotMatrix(~PC1+PC2+PC3|status, data=pc_score_note , reg.line="" , smoother="", col=c("orange","grey") , smoother.args=list(col="grey") , cex=1 , pch=c(15,16))

# pc loadings
loadings<-pc_note$rotation%*%diag(pc_note$sdev)
barplot(loadings[,1])
barplot(loadings[,2])
biplot(pc_note)
pc_note$rotation

# bartlett's sphericity test
install.packages("psych")
library(psych)
cortest.bartlett(cor(bank[,1:6]), n=200)

### Gross state Product data ###
gsp_share<-read.csv("GSP_SHARE.csv", header=T)
gsp_share

pc_gsps<-prcomp(gsp_share[,-1], scale=T)
round(cor(pc_gsps$x),3)
screeplot(pc_gsps, type="l")
loadings<-pc_gsps$rotation%*%diag(pc_gsps$sdev)
loadings
barchart(loadings[,1])
biplot(pc_gsps)
