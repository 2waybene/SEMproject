################## THE 4-VARIABLE INTRO TO LAVAAN EXAMPLE #########
### LAST UPDATED 15.03.26
# Set working directory and load data
setwd("X:/myGit/SEMproject/data") #commands in bold
dat <- read.csv("GSE58144_levels_activities.csv")
library(lavaan)


# Examine 
summary(dat)


# Step 1: Specify model
mod.1 <- 'SOX17_lev.2 ~ GATA2_lev +  PGR_lev'

# Step 1: Specify another model
# How to capture a bi-directional relationship in the model??
# Step 2: Estimate model
mod.1.fit <- sem(mod.1, data=dat)
varTable(mod.1.fit)

# Step 3: Extract results
summary(mod.1.fit) 

summary(mod.1.fit, rsq=T, standardized=T) 

##  second model 
mod.2 <- 'SOX17_lev.2 <~ 1*GATA2_lev +  PGR_lev'

mod.2.fit <- sem(mod.2, data=dat)
# Check variances
varTable(mod.2.fit)

# Step 3: Extract results
summary(mod.2.fit) 
#PGR_lev ~ GATA2_lev'


mod.2.fit <- sem(mod.2, data=dat)



