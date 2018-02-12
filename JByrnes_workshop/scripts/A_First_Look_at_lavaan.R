### A FIRST LOOK AT LAVAAN
# from www.structuralequations.org
# This code accompanies tutorial "A_First_Look_at_lavaan.pdf"

# DATA and example used in this demonstration from
# Grace and Keeley (2006) Ecol. Apps. 16:503-514 
#(http://www.werc.usgs.gov/OLDsitedata/seki/pdfs/k2006_grace_sem_ea.pdf)

# Set your working directory
#setwd("F:/StructuralEquationsDotOrg/Jims_LavaanTutorials/Tutorial1") #commands in bold


setwd("x:/myGit/SEMproject/JByrnes_workshop/data/")


# Load data and name file "k.dat"
k.dat<-read.csv("./Keeley_rawdata_select4.csv")




# Examine contents of keeley data file
names(k.dat)
head(k.dat)


###Load Libraries
library(lavaan)

### Lavaan syntax for a single-equation ################

# Write lavaan code for this single equation model
mod1 <- 'cover ~ age + firesev'

# Fit the model (i.e. est. parameters)
mod1.fit <- sem(mod1, data=k.dat)

# Output a summary of the computed results
summary(mod1.fit, rsq=T)  # rsq=T means output the r-sqr

########################################################

# Fix the data
summary(k.dat$cover); 
summary(k.dat$age); 
summary(k.dat$firesev)

k.dat$age <- k.dat$age/100 # scale age variable
k.dat$firesev <- k.dat$firesev/10 # scale firesev
# then rerun the above model


### Lavaan multi-equation model ################################################
# Lavaan model(separate line for each equation; whole model is enclosed in quotes).

# Model code
mod2 <- 'cover   ~ firesev
firesev ~ age'

# Fit model
mod2.fit <- sem(mod2, data=k.dat)

# Output results
summary(mod2.fit, rsq=T)

#examine standardized parameters
std.params2 <- standardizedSolution(model.lav2.est)
print(std.params2)  #note, std errors, z and p-values are not computed
