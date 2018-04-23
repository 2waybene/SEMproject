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

save (NewDT, file = "parsedNewDT.rda")
save ( CellReportDT, file = "parsedCellReportDT.rda")



load("parsedNewDT.rda")
##=============================
##  Do something here
##==============================

as.dist(cor(k.dat[,c(1:3)])) 
# Examine contents of keeley data file
names(k.dat)
head(k.dat)

cor.test(k.dat$GATA2_lev, k.dat$PGR_lev)
cor.test(k.dat$GATA2_act, k.dat$PGR_act)

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
mod1.fit <- sem(mod1, data=k.dat)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

source("x:/R-project/customPackages/plotTools.R")
(xb <- rcorr(as.matrix(k.dat[,c(1:3)]))) 

########################################################




##======================================
##  Figure 6E
##  p4_act ~ GATA2_lev + PGR_lev
##=====================================
names(k.dat)

# Write lavaan code for this single equation model
mod1 <- 'P4_act ~ GATA2_lev + PGR_lev'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=k.dat)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

source("x:/R-project/customPackages/plotTools.R")
(xb <- rcorr(as.matrix(k.dat[,c(1:2,6)]))) 



##======================================
##  Figure 6D
##  SOX17_lev  ~ GATA2_act + PGR_act
##=====================================
names(k.dat)

# Write lavaan code for this single equation model
mod1 <- 'SOX17_lev  ~ GATA2_act + PGR_act'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=k.dat)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

source("x:/R-project/customPackages/plotTools.R")
(xb <- rcorr(as.matrix(k.dat[,c(3,4,5)]))) 


##======================================
##  p4_act ~ GATA2_act + PGR_act
##  NOT shown in the paper
##=====================================
names(k.dat)

# Write lavaan code for this single equation model
mod1 <- 'P4_act ~ GATA2_act + PGR_act'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=k.dat)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

source("x:/R-project/customPackages/plotTools.R")
(xb <- rcorr(as.matrix(k.dat[,c(4:6)]))) 





