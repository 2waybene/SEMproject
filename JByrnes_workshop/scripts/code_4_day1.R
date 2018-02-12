######################################################################
##### Code for Day 1 of SEM Workshop
##### covering piecewise model fitting
#####
##### Jarrett E.K. Byrnes
#####
##### Last Modified 2/1/13
######################################################################

#Step 1) Set your working Directory
setwd("x:/myGit/SEMproject/JByrnes_workshop/data/")

#Step 2) Load your Data File
keeley<-read.csv("./Keeley_rawdata_select4.csv")

#Step 3) examine the data 
head(keeley)

#Step 4) view the data
pairs(keeley)


#regression syntax versus lavaan
aLM<-lm(cover ~ age, data=keeley)

#F Table
library(car)
Anova(aLM)

#getting coefficients
summary(aLM)

#standardized coefficients
coef(aLM)[2]*sd(keeley$age)/sd(keeley$cover)

#standardized coefficients 2
library(QuantPsyc)
lm.beta(aLM)


#####
#Multiple Regression & SEM
#####

aLM2<-lm(cover ~ age+firesev, data=keeley)

#what to include...
Anova(aLM2)

#note difference with anova!
anova(aLM2)

#standardized coefs
cor(keeley$age, keeley$firesev)
summary(aLM2)$r.squared
lm.beta(aLM2)

#finish the sem
aLM3<-lm(firesev ~ age, data=keeley)
Anova(aLM3)
summary(aLM3)$r.squared
lm.beta(aLM3)

##############################
#####Evaluating Mediation I
##############################

#Refit the new cover relationship
fullMedLM<-lm(cover ~ firesev, data=keeley)
summary(fullMedLM)$r.squared
lm.beta(fullMedLM)

keeley$fireCoverResiduals<-residuals(fullMedLM)
residLM<-lm(fireCoverResiduals ~ age, data=keeley)
Anova(residLM)
plot(fireCoverResiduals ~ age, data=keeley)


################################
### SEM for Exercise
################################

#fit the pieces
abioticLM <- lm(abiotic ~ distance, data=keeley)
heteroLM <- lm(hetero ~ distance, data=keeley)
richnessLM <- lm(rich ~ abiotic + distance + hetero, data=keeley)

#evaluate the pieces
Anova(abioticLM)
Anova(heteroLM)
Anova(richnessLM)


#standardized coefficients & r^2
lm.beta(abioticLM)
lm.beta(heteroLM)
lm.beta(richnessLM)

summary(abioticLM)$r.squared
summary(heteroLM)$r.squared
summary(richnessLM)$r.squared

#####
#Evaluate Mediation
richnessLM2 <- lm(rich ~ abiotic  + hetero, data=keeley)
summary(richnessLM2)$r.squared
lm.beta(richnessLM2)

keeley$richnessResidual<-residuals(richnessLM2)
richResidLM<-lm(richnessResidual ~ distance, data=keeley)
Anova(richResidLM)

#plot result
plot(richnessResidual ~ distance, data=keeley, cex=1.5)
abline(richResidLM, col="red", lwd=2)


##################
# D-Separation
##################

###D-Sep test by hand
summary(richnessLM)$coef

bs2LM<-lm(abiotic ~ hetero + distance, data=keeley)
summary(bs2LM)$coef

#calculate C
fisherC <- -2*(log(9.56e-05) + log(0.187))

#the test
1-pchisq(fisherC, 4)


#Or do it with ggm
library(ggm)

#code the model into a matrix
modelMat<-DAG(abiotic ~ distance, 
              hetero ~ distance, 
              rich ~ abiotic + hetero)

modelMat

#Basis Set
basiSet(modelMat)

#the Shipley Test
shipley.test(modelMat, cov(keeley), n=nrow(keeley))

####################
# Final Exercise
####################

#make a DAG
modelMat2<-DAG(age ~ distance,
               firesev ~ age,
               cover ~ firesev)

#dsep test
shipley.test(modelMat2, cov(keeley), n=nrow(keeley))

#fit model
lm1<-lm(age ~ distance, data=keeley)
lm2<-lm(firesev ~ age, data=keeley)
lm3<-lm(cover ~ firesev, data=keeley)

#evaluate paths
Anova(lm1)
Anova(lm2)
Anova(lm3)

#std coefs
lm.beta(lm1)
lm.beta(lm2)
lm.beta(lm3)

#R^2
summary(lm1)$r.square
summary(lm2)$r.square
summary(lm3)$r.square


####################
# semPlot
####################
library(semPlot)
semPaths(lm1+lm2+lm3, layout="tree2", "std", intercepts=FALSE)

