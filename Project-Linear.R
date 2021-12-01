################## ENV DATA ANALYSIS PROJECT ##################
###############################################################
######################### Linear Model ########################
###############################################################

####### Import Requirments ##

dta <- read_excel("C:/Users/Google Drive/RS Lab/Env Data Analysis/Project/dta3.xlsx")
dta<-data.frame(dta)
cor.matrix(dta[c(9,14,19,20,21,22,23)])


results=data.frame(1,1)

### 1. Naive Model.

##  1.a. Zinc_out

#   1.a.1. Full Model

mod.1.a.1<-lm(ZINC_OUT_.ug.L.~LOAD_Rate_.gpm.sq.ft..+TSS_IN_.mg.L.+
              TSS_OUT_.mg.L.+COPPER_IN_.ug.L.+COPPER_OUT_.ug.L.+
              ZINC_IN_.ug.L., data=dta)
results$Testname<-"Full_ZincOut_Naive"
dummy<-summary(mod.1.a.1)
results$AIC<-AIC(mod.1.a.1)
results$BIC<-BIC(mod.1.a.1)
results$adj.r.squared<-dummy$adj.r.squared

par(mfrow=c(2,2))
plot(mod.1.a.1)

#   1.a.2. Reduced Model

##  1.b. Copper_out

#   1.b.1. Full Model

#   1.b.2. Reduced Model

##  1.c. Removal Rate Model

#   1.c.1. Zinc Removal

#   1.c.2. Copper Removal