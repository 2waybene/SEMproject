<<<<<<< HEAD
##=========================================================================
##  File: IFNAR1_analysis_manuscript_part_1.R
##
##  Author: Jianying Li
##  Purpose: to analyze the association of the genetic effect of
##           a functional/coding SNP "rs2257167" on the disease
##           phenotypes (severe vs. mild), and generate odds
##           ratio and confident interval 
##  Comment: this study was originally from the Golden Gate project
##           from 2016 with major analytical component in the 
##           analysisFuncs.R. There are five panels for genotyping
##           only one "BPIIFA1(PLUNC) to MYD88" has this SNP.
##
##           Genotyp coding scema:
##                0 : homozygous reference allele
##                1 : heterozygous ref/SNP
##                2 : homozygous variant allele
##
##           In the convention, normally 0 refers to the "minor allele"
##           which is supposed to be "the variant allele"
##           but, in our study it turns out our variant allele is MAJOR
##
##  History: originally named IFNAR1_analysis.R    
##           then:IFNAR1_analysis_v2.R
##
#            in this practice, I am elaborating on all the cases:
##  Case-I:  
##        We assumed three genotypes AA AB BB, with these three genotypes
##        We tested: additive effects with glm (logistic model) assuming 
##        genetic mode, 0, 1, 2 case. The p-value will be similar to the 
##        trend test
##        Cohr
##=========================================================================
setwd ("x:/project2018/RSV_association_study/study_with_IFNAR1/")
source ("analysisFuncs.R")


##=========================================
##  Prepare the association analysis data
##=========================================

load("SNP_file_BPIIFA1_to_MYD88.rda")
load("clinic_dt.rda")

SNP_ifnar1 = "seq.rs2257167"
column2use = which(colnames(sheetIN) %in% SNP_ifnar1)
snp.name = SNP_ifnar1
#column2use

SAT_phenotype_col =  6



#snp.name <- colnames(sheetIN)[column2use]
#snp.name

geno.pheno <- cbind(sheetIN[,column2use], dt[,SAT_phenotype_col])
geno.pheno <- as.data.frame(geno.pheno)

##==================================================
##  This is VERY important that for now we HAVE TO 
##  flip the genotype coding! 
##  Genotyp coding scema:
##                0 : homozygous reference allele
##                1 : heterozygous ref/SNP
##                2 : homozygous variant allele
##==================================================
geno.pheno[,1] <-  abs (geno.pheno[,1] - 2)

table (geno.pheno[,1])
#0   1   2 
#396 286  55 

##====================================================
##  OKAY, from now on, we can proceed with the study
##====================================================
colnames(geno.pheno) <- c ("SNP", "SAT")
rownames(geno.pheno) <- dt$RSV.ID

##  Here, we are looking at all the SNP 
##  regardless of RSV infection status
data_all <- geno.pheno
#str(dt$RSV.ID)
#str(geno.pheno)


##  ONLY use RSV positive for the following analyses!!
row2use <-  which(dt$RSV == 1)
geno.pheno <- geno.pheno [row2use ,]
cov.dt <- dt[row2use ,] 

##  Exclude individual with missing SNP information
if (length(which(is.na(geno.pheno[,1]))) > 0) {
  missing.row = which(is.na(geno.pheno[,1]))
  geno.pheno <-  geno.pheno[-missing.row,]
  cov.dt <- cov.dt[-missing.row ,] 
}

str(geno.pheno)
str(cov.dt)


##======================================================
##  Done with prepariing the  association analysis data
##======================================================

##=============================
##  SNP frequency assessment
##=============================

# 1 For all SNP/indivudual

d1 <- as.data.frame(table (data_all[,1]))
REFF <- (d1$Freq[1]*2 + d1$Freq[2])/(sum(d1$Freq)*2)
VARF <- (d1$Freq[3]*2 + d1$Freq[2])/(sum(d1$Freq)*2)

case.1 <- c(REFF , VARF )


# 2 SNP from indivudual with RSV positive

d2 <- as.data.frame(table (geno.pheno[,1]))
REFF2 <- (d2$Freq[1]*2 + d2$Freq[2])/(sum(d2$Freq)*2)
VARF2 <- (d2$Freq[3]*2 + d2$Freq[2])/(sum(d2$Freq)*2)

case.2 <- c(REFF2 , VARF2 )

snp.freq <- as.data.frame (rbind (case.1, case.2))
colnames (snp.freq) <- c("Reference allele (G) frequency", "Variant allele (C) frequency")
rownames (snp.freq) <- c("ALL individual", "Individual with RSV positive")
snp.freq


##=========================================================
##  Testing the additive effects with glm (logistic model) 
##  It is equivelant to our case I
##=========================================================
table(geno.pheno)

table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

##  So, there are more reference genotypes than SNP genotypes

##  If we code it as numeric -- trend test
m <- as.data.frame(geno.pheno)
summary(glm(m[,2] ~ m[,1], family=binomial("logit")))

##  m[, 1]       -0.5037     0.1598  -3.153  0.00162 ** 
##  It reports that there is "additive effect" from the p-value


##  If we code it as factor
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))

# as.factor(m[, 1])1  -0.3757     0.2178  -1.725  0.08456 .  
# as.factor(m[, 1])2  -1.1917     0.3887  -3.066  0.00217 ** 

##  Both genotypes were also signficant comparing to 
##  "0" phenotype -- reference in our case
##  and, the overall trend (from first model) matches with our 
##  independent two factors: -0.3757 & -1.1917

##  Temporary conclusion is that the variant alleles is NEGATIVELy 
##  associated with the disease outcome. In other words, it has 
##  adverse effect on the disease outcome
##===============================================
##  If I would try a Pearson's Chi-squared test
##===============================================

counts  <- as.matrix(table (m))
chisq.test(counts)

##  Pearson's Chi-squared test gives p-value = 0.004097 
##  indicating the significant result

##===============================================
##  Lastly, let's try Cochran-Armitage trend test
##  with multiCA R pacakge
##===============================================
library(multiCA)

multiCA.test(counts)
##  Cochran-Armitage trend test gives: p-value = 0.004097
##  Same as the Pearson's Chi-squared test



##  When I transpose the matrix, the trend test 
##  target was changed, and 
multiCA.test(t(counts))

##  With the same test statistics, the p-value = 0.00143

##  Same as the chi-square test with 1 degree of freedom
##  1-pchisq(10.167, df=1)
##  [1] 0.001429767
##  This is closer to our logistic regression pvalue: 0.00162

##=================================


##=========================================================
##  Now, we are collapsing the heterozygous genotypes
##  And testing the dominant/recessive effects
##  Dominate effect from the variant (Case II: AA vs AB/BB)
##  
##=========================================================
table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

# case 2
##     AA  |   AB/BB 
##    collapse 1 and 2 to 1
##    leave the reference homozygous genotype coded as "0"

m2 <- as.data.frame(geno.pheno)
m2[which(m2[,1] == 2),1] = 1

table (m2[,1])
# 0   1 
# 219 182

m2.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))
m2.logistic= glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit"))

summary(glm(m2[,2] ~ m2[,1], family=binomial("logit")))

summary(glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit")))
##  as.factor(m2[, 1])1  -0.5221     0.2054  -2.542    0.011 *  

##  With the cateory predictor, the analysis was significant
##  as the palue = 0.011, and the variant allele has the 
##  adverse effect on the disease outcome


exp(summary(m2.logistic)$coefficients[2,1])
exp(summary(m2.logistic)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(m2.logistic)$coefficients[2,2])

##  The odds ratio for the case II is:
##  OR: 0.5932672
##  CI (95) : [0.3966436 , 0.8873607]



##=========================================================
##  Now, we are collapsing the heterozygous genotypes
##  And testing the dominant/recessive effects
##  Dominate effect from the variant (Case II: AA/AB /BB)
##  
##=========================================================
table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

# case 2
##     AA  |   AB/BB 
##    collapse 1 and 2 to 1

m3 <- as.data.frame(geno.pheno)
m3[which(m3[,1] == 1),1] = 0

table (m3[,1])
# 0   1 
# 368 33


summary(glm(m3[,2] ~ m3[,1], family=binomial("logit")))
summary(glm(m3[,2] ~ as.factor(m3[,1]), family=binomial("logit")))
##  as.factor(m3[, 1])2  -1.0358     0.3774  -2.744  0.00606 ** 

##  With the cateory predictor, the analysis was significant
##  as the palue = 0.00606, and the variant allele has the 
##  adverse effect on the disease outcome

m3.logistic= glm(m3[,2] ~ as.factor(m3[,1]), family=binomial("logit"))
exp(summary(m3.logistic)$coefficients[2,1])
exp(summary(m3.logistic)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(m3.logistic)$coefficients[2,2])

##  The odds ratio for the case II is:
##  OR: 0.3549402
##  CI (95) : [0.1693910 , 0.7437384]

##==========================================================================
##  Analysis with covariates: gender, region, breastfeeding, social-status
##==========================================================================

str(dt)

table(m)
table(m2)
table(m3)

##==========================================================================
##  Analysis with covariates: gender
##==========================================================================

m.gender <- cbind (m, cov.dt$GENDER)
colnames(m.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ SNP + Gender, data = m.gender, family=binomial("logit")))

m2.gender <- cbind (m2, cov.dt$GENDER)
colnames(m2.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ as.factor(SNP) + Gender, data = m2.gender, family=binomial("logit")))

m3.gender <- cbind (m, cov.dt$GENDER)
colnames(m3.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ as.factor(SNP) + Gender, data = m3.gender, family=binomial("logit")))

##==========================================================================
##  Analysis with covariates: region
##==========================================================================
m.Region <- cbind (m, cov.dt$REGION)
colnames(m.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ SNP + Region, data = m.Region, family=binomial("logit")))

m2.Region <- cbind (m, cov.dt$REGION)
colnames(m2.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ as.factor(SNP) + Region, data = m2.Region, family=binomial("logit")))

m3.Region <- cbind (m, cov.dt$REGION)
colnames(m3.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ as.factor(SNP) + Region, data = m3.Region, family=binomial("logit")))

##==========================================================================
##  Analysis with covariates: breastfeeding
##==========================================================================
m.Breastfeeding <- cbind (m, cov.dt$Breastfeeding)
colnames(m.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ SNP + Breastfeeding, data = m.Breastfeeding, family=binomial("logit")))

m2.Breastfeeding <- cbind (m, cov.dt$Breastfeeding)
colnames(m2.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m2.Breastfeeding, family=binomial("logit")))

m3.Breastfeeding <- cbind (m, cov.dt$Breastfeeding)
colnames(m3.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m3.Breastfeeding, family=binomial("logit")))

##==========================================================================
##  Analysis with covariates: social-status
##==========================================================================
m.SES<- cbind (m, cov.dt$SES)
colnames(m.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ SNP + SES, data = m.SES, family=binomial("logit")))

m2.SES<- cbind (m, cov.dt$SES)
colnames(m2.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ as.factor(SNP) + SES, data = m2.SES, family=binomial("logit")))

m3.SES<- cbind (m, cov.dt$SES)
colnames(m3.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ as.factor(SNP) + SES, data = m3.SES, family=binomial("logit")))




=======
##=========================================================================
##  File: IFNAR1_analysis_manuscript_part_1.R
##
##  Author: Jianying Li
##  Purpose: to analyze the association of the genetic effect of
##           a functional/coding SNP "rs2257167" on the disease
##           phenotypes (severe vs. mild), and generate odds
##           ratio and confident interval 
##  Comment: this study was originally from the Golden Gate project
##           from 2016 with major analytical component in the 
##           analysisFuncs.R. There are five panels for genotyping
##           only one "BPIIFA1(PLUNC) to MYD88" has this SNP.
##
##           Genotyp coding scema:
##                0 : homozygous reference allele
##                1 : heterozygous ref/SNP
##                2 : homozygous variant allele
##
##           In the convention, normally 0 refers to the "minor allele"
##           which is supposed to be "the variant allele"
##           but, in our study it turns out our variant allele is MAJOR
##
##  History: originally named IFNAR1_analysis.R    
##           then:IFNAR1_analysis_v2.R
##
#            in this practice, I am elaborating on all the cases:
##  Case-I:  
##        We assumed three genotypes AA AB BB, with these three genotypes
##        We tested: additive effects with glm (logistic model) assuming 
##        genetic mode, 0, 1, 2 case. The p-value will be similar to the 
##        trend test
##        Cohr
##=========================================================================
setwd ("x:/project2018/RSV_association_study/study_with_IFNAR1/")
source ("analysisFuncs.R")


##=========================================
##  Prepare the association analysis data
##=========================================

load("SNP_file_BPIIFA1_to_MYD88.rda")
load("clinic_dt.rda")

SNP_ifnar1 = "seq.rs2257167"
column2use = which(colnames(sheetIN) %in% SNP_ifnar1)
snp.name = SNP_ifnar1
#column2use

SAT_phenotype_col =  6



#snp.name <- colnames(sheetIN)[column2use]
#snp.name

geno.pheno <- cbind(sheetIN[,column2use], dt[,SAT_phenotype_col])
geno.pheno <- as.data.frame(geno.pheno)

##==================================================
##  This is VERY important that for now we HAVE TO 
##  flip the genotype coding! 
##  Genotyp coding scema:
##                0 : homozygous reference allele
##                1 : heterozygous ref/SNP
##                2 : homozygous variant allele
##==================================================
geno.pheno[,1] <-  abs (geno.pheno[,1] - 2)

table (geno.pheno[,1])
#0   1   2 
#396 286  55 

##====================================================
##  OKAY, from now on, we can proceed with the study
##====================================================
colnames(geno.pheno) <- c ("SNP", "SAT")
rownames(geno.pheno) <- dt$RSV.ID

##  Here, we are looking at all the SNP 
##  regardless of RSV infection status
data_all <- geno.pheno
#str(dt$RSV.ID)
#str(geno.pheno)


##  ONLY use RSV positive for the following analyses!!
row2use <-  which(dt$RSV == 1)
geno.pheno <- geno.pheno [row2use ,]
cov.dt <- dt[row2use ,] 

##  Exclude individual with missing SNP information
if (length(which(is.na(geno.pheno[,1]))) > 0) {
  missing.row = which(is.na(geno.pheno[,1]))
  geno.pheno <-  geno.pheno[-missing.row,]
  cov.dt <- cov.dt[-missing.row ,] 
}

str(geno.pheno)
str(cov.dt)


##======================================================
##  Done with prepariing the  association analysis data
##======================================================

##=============================
##  SNP frequency assessment
##=============================

# 1 For all SNP/indivudual

d1 <- as.data.frame(table (data_all[,1]))
REFF <- (d1$Freq[1]*2 + d1$Freq[2])/(sum(d1$Freq)*2)
VARF <- (d1$Freq[3]*2 + d1$Freq[2])/(sum(d1$Freq)*2)

case.1 <- c(REFF , VARF )


# 2 SNP from indivudual with RSV positive

d2 <- as.data.frame(table (geno.pheno[,1]))
REFF2 <- (d2$Freq[1]*2 + d2$Freq[2])/(sum(d2$Freq)*2)
VARF2 <- (d2$Freq[3]*2 + d2$Freq[2])/(sum(d2$Freq)*2)

case.2 <- c(REFF2 , VARF2 )

snp.freq <- as.data.frame (rbind (case.1, case.2))
colnames (snp.freq) <- c("Reference allele (G) frequency", "Variant allele (C) frequency")
rownames (snp.freq) <- c("ALL individual", "Individual with RSV positive")
snp.freq


##=========================================================
##  Testing the additive effects with glm (logistic model) 
##  It is equivelant to our case I
##=========================================================
table(geno.pheno)

table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

##  So, there are more reference genotypes than SNP genotypes

##  If we code it as numeric -- trend test
m <- as.data.frame(geno.pheno)
summary(glm(m[,2] ~ m[,1], family=binomial("logit")))

##  m[, 1]       -0.5037     0.1598  -3.153  0.00162 ** 
##  It reports that there is "additive effect" from the p-value


##  If we code it as factor
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))

# as.factor(m[, 1])1  -0.3757     0.2178  -1.725  0.08456 .  
# as.factor(m[, 1])2  -1.1917     0.3887  -3.066  0.00217 ** 

##  Both genotypes were also signficant comparing to 
##  "0" phenotype -- reference in our case
##  and, the overall trend (from first model) matches with our 
##  independent two factors: -0.3757 & -1.1917

##  Temporary conclusion is that the variant alleles is NEGATIVELy 
##  associated with the disease outcome. In other words, it has 
##  adverse effect on the disease outcome
##===============================================
##  If I would try a Pearson's Chi-squared test
##===============================================

counts  <- as.matrix(table (m))
chisq.test(counts)

##  Pearson's Chi-squared test gives p-value = 0.004097 
##  indicating the significant result

##===============================================
##  Lastly, let's try Cochran-Armitage trend test
##  with multiCA R pacakge
##===============================================
library(multiCA)

multiCA.test(counts)
##  Cochran-Armitage trend test gives: p-value = 0.004097
##  Same as the Pearson's Chi-squared test



##  When I transpose the matrix, the trend test 
##  target was changed, and 
multiCA.test(t(counts))

##  With the same test statistics, the p-value = 0.00143
##  This is closer to our logistic regression pvalue: 0.00162

##=================================


##=========================================================
##  Now, we are collapsing the heterozygous genotypes
##  And testing the dominant/recessive effects
##  Dominate effect from the variant (Case II: AA vs AB/BB)
##  
##=========================================================
table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

# case 2
##     AA  |   AB/BB 
##    collapse 1 and 2 to 1

m2 <- as.data.frame(geno.pheno)
m2[which(m2[,1] == 2),1] = 1

table (m2[,1])
# 0   1 
# 219 182

m2.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))
m2.logistic= glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit"))

summary(glm(m2[,2] ~ m2[,1], family=binomial("logit")))
summary(glm(m2[,2] ~ 1 , family=binomial("logit")))

summary(glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit")))
##  as.factor(m2[, 1])1  -0.5221     0.2054  -2.542    0.011 *  

##  With the cateory predictor, the analysis was significant
##  as the palue = 0.011, and the variant allele has the 
##  adverse effect on the disease outcome


exp(summary(m2.logistic)$coefficients[2,1])
exp(summary(m2.logistic)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(m2.logistic)$coefficients[2,2])

##  The odds ratio for the case II is:
##  OR: 0.5932672
##  CI (95) : [0.3966436 , 0.8873607]


##  This is a little bit convoluted in explaining this results. First of all, for the outcome severity: 1 means server, 0 means mild.
##  The predictor variable, genotype: 1 means involving variant allel "B", 0 means reference allele "A"

##  So, the odds ratio (@ OR: 0.5932672) means with the genotype involving variant allele, it decreases the serverity event (vss mild). 
##  therefore, should be explained as "protective effect".

##=========================================================
##  Now, we are collapsing the heterozygous genotypes
##  And testing the dominant/recessive effects
##  Dominate effect from the variant (Case II: AA/AB /BB)
##=========================================================

table(geno.pheno[,1])
#   0   1   2 
# 219 149  33 

# case 3
##     AA/AB  |   BB 
##    collapse 1 to 0
##    collapse 2 to 1

m3 <- as.data.frame(geno.pheno)
m3[which(m3[,1] == 1),1] = 0
m3[which(m3[,1] == 2),1] = 1

table (m3[,1])
# 0   1 
# 368 33


summary(glm(m3[,2] ~ m3[,1], family=binomial("logit")))
summary(glm(m3[,2] ~ as.factor(m3[,1]), family=binomial("logit")))
##  as.factor(m3[, 1])2  -1.0358     0.3774  -2.744  0.00606 ** 

##  With the cateory predictor, the analysis was significant
##  as the palue = 0.00606, and the variant allele has the 
##  adverse effect on the disease outcome

m3.logistic= glm(m3[,2] ~ as.factor(m3[,1]), family=binomial("logit"))
exp(summary(m3.logistic)$coefficients[2,1])
exp(summary(m3.logistic)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(m3.logistic)$coefficients[2,2])

##  The odds ratio for the case II is:
##  OR: 0.3549402
##  CI (95) : [0.1693910 , 0.7437384]

##  So, the odds ratio (@ OR: 0.3549402) means with the genotype involving variant allele, it decreases the serverity event (vss mild). 
##  therefore, should be explained as "protective effect".


##==========================================================================
##  Analysis with covariates: gender, region, breastfeeding, social-status
##==========================================================================

str(dt)

table(m)
table(m2)
table(m3)

##==========================================================================
##  Analysis with covariates: gender
##==========================================================================

m.gender <- cbind (m, cov.dt$GENDER)
colnames(m.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ SNP + Gender, data = m.gender, family=binomial("logit")))
logit <- glm(SAT ~ SNP + Gender, data = m.gender, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


m2.gender <- cbind (m2, cov.dt$GENDER)
colnames(m2.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ as.factor(SNP) + Gender, data = m2.gender, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + Gender, data = m2.gender, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


m3.gender <- cbind (m3, cov.dt$GENDER)
colnames(m3.gender) <- c (colnames(m), "Gender")
summary(glm(SAT ~ as.factor(SNP) + Gender, data = m3.gender, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + Gender, data = m3.gender, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


##==========================================================================
##  Analysis with covariates: region
##==========================================================================
m.Region <- cbind (m, cov.dt$REGION)
colnames(m.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ SNP + as.factor(Region), data = m.Region, family=binomial("logit")))
logit <- glm(SAT ~ SNP + as.factor(Region),data = m.Region, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])



m2.Region <- cbind (m2, cov.dt$REGION)
colnames(m2.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ as.factor(SNP) + as.factor(Region), data = m2.Region, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + as.factor(Region), data = m2.Region, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])



m3.Region <- cbind (m3, cov.dt$REGION)
colnames(m3.Region) <- c (colnames(m), "Region")
summary(glm(SAT ~ as.factor(SNP) + as.factor(Region), data = m3.Region, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + as.factor(Region), data = m3.Region, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


##==========================================================================
##  Analysis with covariates: breastfeeding
##==========================================================================
m.Breastfeeding <- cbind (m, cov.dt$Breastfeeding)
colnames(m.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ SNP + Breastfeeding, data = m.Breastfeeding, family=binomial("logit")))
logit <- glm(SAT ~ SNP + Breastfeeding, data = m.Breastfeeding, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


m2.Breastfeeding <- cbind (m2, cov.dt$Breastfeeding)
colnames(m2.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m2.Breastfeeding, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m2.Breastfeeding, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


m3.Breastfeeding <- cbind (m3, cov.dt$Breastfeeding)
colnames(m3.Breastfeeding) <- c (colnames(m), "Breastfeeding")
summary(glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m3.Breastfeeding, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + Breastfeeding, data = m3.Breastfeeding, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])



##==========================================================================
##  Analysis with covariates: social-status
##==========================================================================
m.SES<- cbind (m, cov.dt$SES)
colnames(m.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ SNP + SES, data = m.SES, family=binomial("logit")))
logit <- glm(SAT ~ SNP + SES, data = m.SES, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])



m2.SES<- cbind (m2, cov.dt$SES)
colnames(m2.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ as.factor(SNP) + SES, data = m2.SES, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + SES, data = m2.SES, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])


m3.SES<- cbind (m3, cov.dt$SES)
colnames(m3.SES) <- c (colnames(m), "SES")
summary(glm(SAT ~ as.factor(SNP) + SES, data = m3.SES, family=binomial("logit")))
logit <- glm(SAT ~ as.factor(SNP) + SES, data = m3.SES, family=binomial("logit"))
exp(summary(logit)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(logit)$coefficients[2,2])




>>>>>>> 4f777fd56550872ad990ec801c5edc6894687381
