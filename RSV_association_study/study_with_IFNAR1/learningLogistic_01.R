set.seed(1234567)
##create two binary vectors of length 100 
x=sample(c(0,1),100, replace=T)
y=sample(c(0,1),100, replace=T)

 ##create a 2x2 table with counts
 xytab=table(x,y)
 xytab

 
 count=cbind(xytab[,2],xytab[,1])
 count
 
 
xfactor=factor(c("0","1"))
xfactor

tmp3=glm(count ~ xfactor, family=binomial("logit"))
tmp3


xydata=cbind(x,y)
xydata ## 100 rows, we are showing first 7

tmp1=glm(y~x, family=binomial("logit"))
tmp1
