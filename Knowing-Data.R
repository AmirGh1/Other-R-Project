################## ENV DATA ANALYSIS PROJECT ##################
###############################################################
######################### Knowing Data ########################
###############################################################

####### Import Requirments ##
install.packages("ggplot2")
library(ggplot2)
dta <- read_excel("C:/Users/Google Drive/RS Lab/Env Data Analysis/Project/dta.xlsx")
dta<-data.frame(dta)
source("C:\\Users\\Google Drive\\RS Lab\\Env Data Analysis\\Worksheets\\cor.matrix.r")



#############################

cor.matrix(dta[c(4,6,8,10,12,15,17)])
#par(mfrow=c(2,7))
lyot<-rbind(c(1:7),c(8:14))
htlo<-rbind(rep(2,7),rep(1,7))
layout(lyot,  heights = htlo)
#layout.show(nf)

par(cex.lab=1.5)
par(cex.lab=1.5)

boxplot(dta[,4], main="BoxPlot - Load rate")
boxplot(dta[,6], main="BoxPlot - TSS in")
boxplot(dta[,8], main="BoxPlot - TSS out")
boxplot(dta[,10], main="BoxPlot - Copper in")
boxplot(dta[,12], main="BoxPlot - Copper out")
boxplot(dta[,15], main="BoxPlot - Zinc in")
boxplot(dta[,17], main="BoxPlot - Zinc out")


desc.stat<-matrix(data=NA, nrow = 3, ncol =7, dimnames = list(c("Mean", "SD", "Normality"),
                                                              c("Load rate", "TSS in", "TSS out",
                                                                "Copper in", "Copper out","Zinc in",
                                                                "Zinc out")) )

desc.stat[1,1]<-mean(dta[,4])
desc.stat[1,2]<-mean(dta[,6])
desc.stat[1,3]<-mean(dta[complete.cases(dta[, 8]),8])
desc.stat[1,4]<-mean(dta[,10])
desc.stat[1,5]<-mean(dta[,12])
desc.stat[1,6]<-mean(dta[,15])
desc.stat[1,7]<-mean(dta[,17])

desc.stat[2,1]<-sd(dta[,4])
desc.stat[2,2]<-sd(dta[,6])
desc.stat[2,3]<-sd(dta[complete.cases(dta[, 8]),8])
desc.stat[2,4]<-sd(dta[,10])
desc.stat[2,5]<-sd(dta[,12])
desc.stat[2,6]<-sd(dta[,15])
desc.stat[2,7]<-sd(dta[,17])



qqnorm(dta[,4])
qqline(dta[,4], col="red")
dummy1<-shapiro.test(dta[,4])
text(0,28,paste("P-value:",formatC(dummy1$p.value, format = "e", digits = 2)))
desc.stat[3,1]<-dummy1$p.value>0.05

qqnorm(dta[,6])
qqline(dta[,6], col="red")
dummy2<-shapiro.test(dta[,6])
text(0,250,paste("P-value:",formatC(dummy2$p.value, format = "e", digits = 2)))
desc.stat[3,2]<-dummy2$p.value>0.05

qqnorm(dta[,8])
qqline(dta[,8], col="red")
dummy3<-shapiro.test(dta[,8])
text(0,90,paste("P-value:",formatC(dummy3$p.value, format = "e", digits = 2)))
desc.stat[3,3]<-dummy3$p.value>0.05

qqnorm(dta[,10])
qqline(dta[,10], col="red")
dummy4<-shapiro.test(dta[,10])
text(0,3300,paste("P-value:",formatC(dummy4$p.value, format = "e", digits = 2)))
desc.stat[3,4]<-dummy4$p.value>0.05

qqnorm(dta[,12])
qqline(dta[,12], col="red")
dummy5<-shapiro.test(dta[,12])
text(0,400,paste("P-value:",formatC(dummy5$p.value, format = "e", digits = 2)))
desc.stat[3,5]<-dummy5$p.value>0.05

qqnorm(dta[,15])
qqline(dta[,15], col="red")
dummy6<-shapiro.test(dta[,15])
text(0,25000,paste("P-value:",formatC(dummy6$p.value, format = "e", digits = 2)))
desc.stat[3,6]<-dummy6$p.value>0.05

qqnorm(dta[,17])
qqline(dta[,17], col="red")
dummy7<-shapiro.test(dta[,17])
text(0,2000,paste("P-value:",formatC(dummy7$p.value, format = "e", digits = 2)))
desc.stat[3,7]<-dummy7$p.value>0.05
####################################################################################
norm.results=data.frame( Variable=character(0), W=numeric(0), P-value=numeric(0))
colnames(norm.results)<-c("Variable","W","P-value")

glm(res~., link="log")
  names(dta[c(4,6,8,10,12,15,17)])