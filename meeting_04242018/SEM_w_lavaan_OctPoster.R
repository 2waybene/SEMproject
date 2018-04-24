### A FIRST LOOK AT LAVAAN
# from www.structuralequations.org
# This code accompanies tutorial "A_First_Look_at_lavaan.pdf"

# DATA and example used in this demonstration from
# Grace and Keeley (2006) Ecol. Apps. 16:503-514 
#(http://www.werc.usgs.gov/OLDsitedata/seki/pdfs/k2006_grace_sem_ea.pdf)

# Set your working directory
#setwd("F:/StructuralEquationsDotOrg/Jims_LavaanTutorials/Tutorial1") #commands in bold


##================================================================
##  File: SEM_w_lavaan_confirmation.R
##  Author: Jianying Li
##  Comment: for Drs. Steve Wu and Francesco DeMayo's project
##  Credit: using the LAVAAN package 
##===============================================================


##===============================================================
##  Request from Steve
##  Please use the 238 genes that are listed in Table 1 
##  of enclosed paper as the testing set and look for anything 
##  that may associate with the GATA2-PGR network. Let's first 
##  try model fittings  based on the Cell Report paper Figure 
##  6 C and D because they are relatively simple. Ty has all 
##  the original data if you need any. The results could be a 
##  poster presentation for our BSC review on October 28 this year.
##===============================================================

setwd("x:/project2018/Diagram_Wu/meeting_04242018/")


# Load data and name file "k.dat"
k.dat<-read.csv("x:/project2018/Diagram_Wu/meeting_04242018/diaz-table1-remove-trailings-w-GSE58144-plus-four-genes.csv")

##====================================
##  variance covariance structure
##====================================
head(k.dat)
#pairs(k.dat)
#cov(k.dat)
dim(k.dat)

CellReportDT <- k.dat[,c(2:10)]
head(CellReportDT)
row.names(CellReportDT) <- k.dat[,1]

cor(CellReportDT)
pairs(CellReportDT)



NewDT <- k.dat[,c(11:215)]
row.names(NewDT) <- k.dat[,1]
dim(NewDT)
colnames(NewDT)

save (list(newDT = NewDT, cellDT = CellReportDT), file = "parsedDT.rda")

#save (NewDT, file = "parsedNewDT.rda")
#save ( CellReportDT, file = "parsedCellReportDT.rda")

##=============================
##  Going forward
##============================

setwd("/Users/li11/myGit/SEMproject/meeting_04242018/")
load("parsedNewDT.rda")
dim(NewDT)
colnames(NewDT)

load ("parsedCellReportDT.rda")
head(CellReportDT)
##=============================
##  Do something here
##==============================



colnames(NewDT)
colnames(CellReportDT)

cor.test(CellReportDT$GATA2_lev, CellReportDT$PGR_lev)
cor.test(CellReportDT$GATA2_act, CellReportDT$PGR_act)

###Load Libraries
library(lavaan)


##  NOW, confirming the results in the CellReport paper
##======================================
##  Figure 6C
##  sox17_lev ~ GATA2_lev + PGR_lev
##=====================================

# Write lavaan code for this single equation model
mod1 <- 'SOX17_lev ~ GATA2_lev + PGR_lev'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=CellReportDT)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

#source("x:/R-project/customPackages/plotTools.R")
#(xb <- rcorr(as.matrix(CellReportDT[,c(1:3)]))) 

########################################################




##======================================
##  Figure 6E
##  p4_act ~ GATA2_lev + PGR_lev
##=====================================
colnames(CellReportDT)

# Write lavaan code for this single equation model
mod1 <- 'P4_act ~ GATA2_lev + PGR_lev'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=CellReportDT)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

#source("x:/R-project/customPackages/plotTools.R")
#(xb <- rcorr(as.matrix(CellReportDT[,c(1:2,6)]))) 



##======================================
##  Figure 6D
##  SOX17_lev  ~ GATA2_act + PGR_act
##=====================================
colnames(CellReportDT)

# Write lavaan code for this single equation model
mod1 <- 'SOX17_lev  ~ GATA2_act + PGR_act'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=CellReportDT)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

#source("x:/R-project/customPackages/plotTools.R")
#(xb <- rcorr(as.matrix(CellReportDT[,c(3,4,5)]))) 


##======================================
##  p4_act ~ GATA2_act + PGR_act
##  NOT shown in the paper
##=====================================
colnames(CellReportDT)

# Write lavaan code for this single equation model
mod1 <- 'P4_act ~ GATA2_act + PGR_act'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=CellReportDT)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

#source("x:/R-project/customPackages/plotTools.R")
#(xb <- rcorr(as.matrix(CellReportDT[,c(4:6)]))) 


##=====================================================
##  Select genes that are highly associated with 
##  GATA2
##=====================================================
dim(NewDT)
colnames(NewDT)
head(CellReportDT)

results.final <- NULL

for (k in 1:dim(NewDT)[2])
{
  tempTest <- cor.test (CellReportDT$GATA2, NewDT[,k])
  #if (tempTest$p.value < 0.05){
    result <- list (gene = colnames(NewDT)[k], pval = tempTest$p.value)
  #}
  if (k == 1){
    results.final <- unlist(result)
  }else{
    results.final <- rbind (results.final,unlist(result))
  }
}

corr.w.GATA2 <- results.final[order(as.numeric(results.final[,2])),]

X = -log(as.numeric(corr.w.GATA2[,2]))
hist(X, prob=TRUE, main = "Correlation to GATA2 level", xlab = "Negative logPval",col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults




##=====================================================
##  Select genes that are highly associated with 
##  PGR
##=====================================================
dim(NewDT)
colnames(NewDT)
head(CellReportDT)

results.final <- NULL

for (k in 1:dim(NewDT)[2])
{
  tempTest <- cor.test (CellReportDT$PGR, NewDT[,k])
  #if (tempTest$p.value < 0.05){
  result <- list (gene = colnames(NewDT)[k], pval = tempTest$p.value)
  #}
  if (k == 1){
    results.final <- unlist(result)
  }else{
    results.final <- rbind (results.final,unlist(result))
  }
}

corr.w.PGR <- results.final[order(as.numeric(results.final[,2])),]

X = -log(as.numeric(corr.w.PGR[,2]))
hist(X, prob=TRUE, main = "Correlation to PGR level", xlab = "Negative logPval",col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults



##=====================================================
##  Select genes that are highly associated with 
##  SOX17
##=====================================================
dim(NewDT)
colnames(NewDT)
head(CellReportDT)

results.final <- NULL

for (k in 1:dim(NewDT)[2])
{
  tempTest <- cor.test (CellReportDT$SOX17, NewDT[,k])
  #if (tempTest$p.value < 0.05){
  result <- list (gene = colnames(NewDT)[k], pval = tempTest$p.value)
  #}
  if (k == 1){
    results.final <- unlist(result)
  }else{
    results.final <- rbind (results.final,unlist(result))
  }
}

corr.w.SOX17 <- results.final[order(as.numeric(results.final[,2])),]

X = -log(as.numeric(corr.w.SOX17[,2]))
hist(X, prob=TRUE, main = "Correlation to SOX17 level", xlab = "Negative logPval",col="grey")# prob=TRUE for probabilities not counts
lines(density(X), col="blue", lwd=2) # add a density estimate with defaults


##================
##  Test model
##=================

d.model <- cbind(CellReportDT, NewDT$TACC3)
colnames(d.model)[10] <- "TACC3"

d.model <- cbind(CellReportDT, NewDT$MSX1)
colnames(d.model)[10] <- "MSX1"

# Write lavaan code for this single equation model
mod1 <- 'TACC3  ~ GATA2_act + PGR_act'
mod1 <- 'TACC3  ~ GATA2_lev + PGR_lev'


mod1 <- 'MSX1  ~ GATA2_act + PGR_act'
mod1 <- 'MSX1  ~ GATA2_lev + PGR_lev'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=d.model)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr
