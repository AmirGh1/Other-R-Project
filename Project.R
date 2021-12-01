dta <- read_excel("C:/Users/Amen/Google Drive/RS Lab/Env Data Analysis/Project/dta.xlsx")
dta<-data.frame(dta)
dta.cal<-dta[30:45,]
dta$TotRedPer<-(dta$COPPER_OUT_RedPer+dta$ZINC_OUT_RedPer)/2
dta.cal<-dta[30:45,]

library(lattice) #view individual regression lines
xyplot(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer|Name,data = dta, 
       panel = function(x,y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       })

xyplot(ZINC_OUT_RedPer~TSS_OUT_RedPer|Test.No.,data = dta, 
       panel = function(x,y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       })


par(mfrow=c(1,3))
acf(dta.cal$COPPER_OUT_.ug.L., type = "p")
acf(dta.cal$ZINC_OUT_.ug.L., type = "p")
acf(dta.cal$TSS_OUT_.mg.L., type = "p")

acf(dta.cal$COPPER_OUT_.ug.L.)
acf(dta.cal$ZINC_OUT_.ug.L.)
acf(dta.cal$TSS_OUT_.mg.L.)

par(mfrow=c(1,2))
acf(dta.cal$COPPER_OUT_RedPer, type = "p")
acf(dta.cal$ZINC_OUT_RedPer, type = "p")

par(mfrow=c(1,1))
acf(dta.cal$TotRedPer)
plot(1:16,dta.cal$TotRedPer)
plot(lm(1:16~dta.cal$TotRedPer))
source("C:\\Users\\AmIn\\Google Drive\\RS Lab\\Fall2017\\Env Data Analysis\\Worksheets\\WS9-Oct31\\cor.matrix.r")

cor.matrix(dta.cal[c(9,14,19)])
mod<-lm(dta.cal$TSS_OUT_RedPer~dta.cal$log(ZINC_OUT_RedPer)+log(dta.cal$COPPER_OUT_RedPer))
par(mfrow=c(2,2))
plot(mod)

summary(mod)


dta.1<-subset(dta, Test.No. == '1' | Test.No.=='2')
mod<-lm(dta.1$TSS_OUT_RedPer~dta.1$ZINC_OUT_RedPer+dta.1$COPPER_OUT_RedPer)
par(mfrow=c(2,2))
plot(mod)
summary(mod)
cor.matrix(dta.1[c(9,14,19)])

avg.mod<-lm(dta.1$TSS_OUT_RedPer~dta.1$TotRedPer)
plot(avg.mod)
summary(avg.mod)

library(lme4)
mod.me1<-lmer(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer+(1|Name/Test.No.),data = dta)
mod.me2<-lmer(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer+(1+COPPER_OUT_RedPer|Name),data = dta)
mod.me3<-lmer(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer+(1|Name),data = dta)
mod.me4<-lmer(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer+(1|Test.No.),data = dta)

install.packages("arm")
library(arm)
display(mod.me1)
display(mod.me2)
display(mod.me3)
display(mod.me4)
require(lattice)
xyplot(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer|Name+Test.No.,data = dta, 
       auto.key = list(x = .85, y = .035, corner = c(0, 0)), 
       main = "Extroversion by School and Class") 
xyplot(ZINC_OUT_RedPer~TSS_OUT_RedPer+COPPER_OUT_RedPer|Name,data = dta, 
       auto.key = list(x = .85, y = .035, corner = c(0, 0)), 
       main = "Extroversion by School and Class") 
plot(mod.me4)


install.packages("glmmLasso")
library(glmmLasso)

glmmLasso(, rnd = list(Name=~1), 
          lambda=10, data = dta)
