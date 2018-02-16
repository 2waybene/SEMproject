library(sem)


##  Example 1
# -------------------- Wheaton et al. alienation data ----------------------

S.wh <- readMoments(names=c('Anomia67','Powerless67','Anomia71','Powerless71','Education','SEI'))
11.834
6.947 9.364
6.819 5.091 12.532
4.783 5.028 7.495 9.986
-3.839 -3.889 -3.841 -3.625 9.610
-21.899 -18.831 -21.748 -18.775 35.522 450.288

# This is the model in the SAS manual for PROC CALIS: A Recursive SEM with
# latent endogenous and exogenous variables.
# Curiously, both factor loadings for two of the latent variables are fixed.


model.wh.1 <- specifyModel()
Alienation67 -> Anomia67, NA, 1
Alienation67 -> Powerless67, NA, 0.833
Alienation71 -> Anomia71, NA, 1
Alienation71 -> Powerless71, NA, 0.833
SES -> Education, NA, 1
SES -> SEI, lamb, NA
SES -> Alienation67, gam1, NA
Alienation67 -> Alienation71, beta, NA
SES -> Alienation71, gam2, NA
Anomia67 <-> Anomia67, the1, NA
Anomia71 <-> Anomia71, the1, NA
Powerless67 <-> Powerless67, the2, NA
Powerless71 <-> Powerless71, the2, NA
Education <-> Education, the3, NA
SEI <-> SEI, the4, NA
Anomia67 <-> Anomia71, the5, NA
Powerless67 <-> Powerless71, the5, NA
Alienation67 <-> Alienation67, psi1, NA
Alienation71 <-> Alienation71, psi2, NA
SES <-> SES, phi, NA

sem.wh.1 <- sem(model.wh.1, S.wh, 932)


summary(sem.wh.1, digits = 3)



##  Example 2
##==========================================
R.DHP <- readMoments(diag=FALSE, names=c("ROccAsp", "REdAsp", "FOccAsp",
                                         "FEdAsp", "RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp"))
.6247
.3269 .3669
.4216 .3275 .6404
.2137 .2742 .1124 .0839
.4105 .4043 .2903 .2598 .1839
.3240 .4047 .3054 .2786 .0489 .2220
.2930 .2407 .4105 .3607 .0186 .1861 .2707
.2995 .2863 .5191 .5007 .0782 .3355 .2302 .2950
.0760 .0702 .2784 .1988 .1147 .1021 .0931 -.0438 .2087

R.DHP


