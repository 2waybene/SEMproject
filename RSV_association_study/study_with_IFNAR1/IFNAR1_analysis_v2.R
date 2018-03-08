##=========================================================================
##  File: IFNAR1_analysis_v2.R
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
geno.pheno <-  abs (geno.pheno - 2)

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


##  Exclude individual with missing SNP information
if (length(which(is.na(geno.pheno[,1]))) > 0) {
  geno.pheno <-  geno.pheno[-which(is.na(geno.pheno[,1])),]
}

str(geno.pheno)

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

##  If we code it as numeric -- trend test
m <- as.data.frame(geno.pheno)
summary(glm(m[,2] ~ m[,1], family=binomial("logit")))

##  m[, 1]        0.5037     0.1598   3.153  0.00162 **
##  It reports that there is "additive effect" from the p-value


##  If we code it as factor
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))

#  as.factor(m[, 1])1   0.8160     0.3978   2.051  0.04023 * 
#  as.factor(m[, 1])2   1.1917     0.3887   3.066  0.00217 **

##  Both genotypes were also signficant comparing to 
##  "0" phenotype -- reference in our case
##  and, the overall trend (from first model) matches with our 
##  independent two factors: 0.8160 & 1.1917


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


# case 2
##     AA  |   AB/BB 
##    Convert "1" to "2"

m <- as.data.frame(geno.pheno)
m[which(m[,1] == 1),1] = 2 
m[which(m[,1] == 2),1] = 1

m2 <- m
tmp.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))
tmp.logistic= glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit"))

summary(glm(m2[,2] ~ m2[,1], family=binomial("logit")))
summary(glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit")))


exp(summary(tmp.logistic)$coefficients[2,1] +  qnorm(c(0.025,0.5,0.975)) *summary(tmp.logistic)$coefficients[2,2])
exp(summary(tmp.logistic)$coefficients[2,1])


##  The odds ratio for the 


sig.chisq.case.I <- NULL
sig.chisq.case.II <- NULL
sig.chisq.case.III <- NULL

# case 1   
##  |   AA  |   AB    |  BB   |
##  Keep all three phenotypes



m2 <- data.frame(sapply(m, function(x) as.numeric(as.character(x))))
tmp.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))

if ( !is.na(as.numeric(coef(summary(tmp.logistic))[, 4][2]))  & as.numeric(coef(summary(tmp.logistic))[, 4][2]) < 0.05 )
##  a signficant result
{
  sig.chisq <- list ("SNP" = snp.name, pval =   as.numeric(coef(summary(tmp.logistic))[,4][2]))
  if (is.null(sig.chisq.case.I))
  {
    sig.chisq.case.I <- as.data.frame(sig.chisq)
  }else{
    sig.chisq.case.I  = rbind (sig.chisq.case.I ,as.data.frame(sig.chisq))
  }
}

# case 2
##     AA  |   AB/BB 
##    Convert "1" to "2"

m <- as.data.frame(geno.pheno)
m[which(m[,1] == 1),1] = 2 
m2 <- data.frame(sapply(m, function(x) as.numeric(as.character(x))))
tmp.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))
tmp.logistic= glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit"))

summary(glm(m2[,2] ~ m2[,1], family=binomial("logit")))
summary(glm(m2[,2] ~ as.factor(m2[,1]), family=binomial("logit")))

if ( !is.na(as.numeric(coef(summary(tmp.logistic))[, 4][2]))  & as.numeric(coef(summary(tmp.logistic))[, 4][2]) < 0.05 )
{
  sig.chisq <- list ("SNP" = snp.name, pval =   as.numeric(coef(summary(tmp.logistic))[,4][2]))
  if (is.null(sig.chisq.case.II))
  {
    sig.chisq.case.II <- as.data.frame(sig.chisq)
  }else{
    sig.chisq.case.II  = rbind (sig.chisq.case.II ,as.data.frame(sig.chisq))
  }
}

# case 3
##    AA/AB |   BB   |
##    Convert "1" to "0"

m <- as.data.frame(geno.pheno)
m[which(m[,1] == 1),1] = 0
m2 <- data.frame(sapply(m, function(x) as.numeric(as.character(x))))
tmp.logistic= glm(m2[,2] ~ m2[,1], family=binomial("logit"))

if ( !is.na(as.numeric(coef(summary(tmp.logistic))[, 4][2]))  & as.numeric(coef(summary(tmp.logistic))[, 4][2]) < 0.05 )
{
  sig.chisq <- list ("SNP" = snp.name, pval =   as.numeric(coef(summary(tmp.logistic))[,4][2]))
  if (is.null(sig.chisq.case.III))
  {
    sig.chisq.case.III <- as.data.frame(sig.chisq)
  }else{
    sig.chisq.case.III  = rbind (sig.chisq.case.III ,as.data.frame(sig.chisq))
  }
}


sig.chisq.case.I 
sig.chisq.case.II 
sig.chisq.case.III 
##======================================================
##  End of the initial 3 cases format
##======================================================


##======================================================
##  Allele effect vs. genotyp effect
##  Odds ratio and confident interval
##  Our assumption is:
##  AA: 2   --- homozygous reference
##  AB: 1   --- heterozygous
##  BB: 0   --- homozygous SNP
##======================================================

##====================================
##  Case 1   
##  |   AA  |   AB    |  BB   |
##  Keep all three phenotypes
##=====================================

m <- as.data.frame(geno.pheno)

summary(glm(m[,2] ~ m[,1], family=binomial("logit")))
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))

##  chi-square test

snp.geno.1 <- geno.pheno [which(geno.pheno [,2] == 1),1]
snp.geno.0 <- geno.pheno [which(geno.pheno [,2] == 0),1]

b.1 <- c(length(which(snp.geno.1 == 0)) , length(which(snp.geno.1 == 1)), length(which(snp.geno.1 == 2)))
b.0 <- c(length(which(snp.geno.0 == 0)) , length(which(snp.geno.0 == 1)), length(which(snp.geno.0 == 2)))   


# case 1
dm <- as.data.frame (rbind (b.1, b.0))
colnames(dm) <- c("BB", "AB", "AA")
rownames(dm) <- c ("case-1", "case-0")


chisq.test(dm)$p.value 
chisq.test(dm)

##==============================
##     case 2
##     AA  |   AB/BB 
##     Convert "2" to "1"
##     Recessive variant
##==============================

m <- as.data.frame(geno.pheno)

table(m[,1])

m[which(m[,1] == 2),1] = 1 

summary(glm(m[,2] ~ m[,1], family=binomial("logit")))
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))



# case 3
##    AA/AB |   BB   |
##    Convert "1" to "0"

m <- as.data.frame(geno.pheno)
m[which(m[,1] == 1),1] = 0
m[which(m[,1] == 2),1] = 1

summary(glm(m[,2] ~ m[,1], family=binomial("logit")))
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))



##  chi-square test 

geno.pheno 

snp.geno.1 <- geno.pheno [which(geno.pheno [,2] == 1),1]
snp.geno.0 <- geno.pheno [which(geno.pheno [,2] == 0),1]

b.1 <- c(length(which(snp.geno.1 == 0)) , length(which(snp.geno.1 == 1)), length(which(snp.geno.1 == 2)))
b.0 <- c(length(which(snp.geno.0 == 0)) , length(which(snp.geno.0 == 1)), length(which(snp.geno.0 == 2)))   


# case 1
dm <- as.data.frame (rbind (b.1, b.0))
chisq.test(dm)


colnames(dm) <- c("AA", "AB", "BB")
rownames(dm) <- c ("case-1", "case-0")
if ( !is.na (chisq.test(dm)$p.value) & chisq.test(dm)$p.value < 0.05)
{
  sig.chisq <- list ("SNP" = snp.name, pval =  chisq.test(dm)$p.value)
  if (is.null(sig.chisq.case.I))
  {
    sig.chisq.case.I <- as.data.frame(sig.chisq)
  }else{
    sig.chisq.case.I  = rbind (sig.chisq.case.I ,as.data.frame(sig.chisq))
  }
}



