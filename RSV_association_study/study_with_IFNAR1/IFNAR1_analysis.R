##==================================================================
##  File: IFNAR1_analysis.R
##  Author: Jianying Li
##  Purpose: to analyze the association of the genetic effect of
##           a functional/coding SNP "rs2257167" on the disease
##           phenotypes (severe vs. mild), and generate odds
##           ratio and confident interval 
##  Comment: this study was originally from the Golden Gate project
##           from 2016 with major analytical component in the 
##           analysisFuncs.R
##==================================================================
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
colnames(geno.pheno) <- c ("SNP", "SAT")
rownames(geno.pheno) <- dt$RSV.ID

#str(dt$RSV.ID)
#str(geno.pheno)

##  ONLY use RSV positive
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



##======================================================
##  Initial 3 cases format
##======================================================
sig.chisq.case.I <- NULL
sig.chisq.case.II <- NULL
sig.chisq.case.III <- NULL

# case 1   
##  |   AA  |   AB    |  BB   |
##  Keep all three phenotypes

m <- as.data.frame(geno.pheno)

summary(glm(m[,2] ~ m[,1], family=binomial("logit")))
summary(glm(m[,2] ~ as.factor(m[,1]), family=binomial("logit")))

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



