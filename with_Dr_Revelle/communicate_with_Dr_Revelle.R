##=====================================================================================================
##  
##  communicate_with_Dr_Revelle.R
##  The following example is from: http://personality-project.org/r/r.sem.html
##  provided by Dr William Revelle at North Western University - Department of Psychology 
##  The sample code was used to demostrate R package SEM, which is very good package
##  for the Structural Equation Model. 
##  
##  But, when I tried the first example, I encountered an error "Incorrect number of dimension"
##  Through a google search, I found the same error reported by others:
##  https://stackoverflow.com/questions/46873264/r-sem-error-incorrect-number-of-dimensions
##  Here it is the example
##====================================================================================================

library(sem)
#Loehlin problem 2.5 
obs.var2.5 = c('Ach1',  'Ach2',  'Amb1',  'Amb2',  'Amb3')

R.prob2.5 = matrix(c(
  1.00 ,  .60  , .30,  .20,   .20,                                               
  .60,  1.00,   .20,   .30,   .10,                                                
  .30,   .20,  1.00,   .70,   .60 ,                                               
  .20,   .30,   .70,  1.00,   .50,                                                
  .20,   .10,   .60,  .50,  1.00), ncol=5,byrow=TRUE)    



#correlated factors structure (ambition <-> Achievement) 
model2.5=matrix(c(
  'Ambit ->  Amb1',      'a', NA,
  'Ambit -> Amb2' ,      'b', NA,
  'Ambit -> Amb3' ,      'c', NA,
  'Achieve -> Ach1',     'd', NA,
  'Achieve -> Ach2',     'e', NA,
  'Ambit <-> Achieve',   'f', NA,
  'Amb1 <-> Amb1' ,      'u', NA,
  'Amb2 <-> Amb2' ,      'v', NA,
  'Amb3 <-> Amb3' ,      'w', NA,
  'Ach1 <-> Ach1' ,      'x', NA,
  'Ach2 <-> Ach2' ,      'y', NA,
  'Achieve <-> Achieve',  NA, 1,
  'Ambit <-> Ambit',      NA, 1),
  ncol=3, byrow=TRUE)


#Dr. Revelle's suggestion, 
colnames(R.prob2.5 ) <- c('Ach1',  'Ach2',  'Amb1',  'Amb2',  'Amb3')
rownames(R.prob2.5 ) <- c('Ach1',  'Ach2',  'Amb1',  'Amb2',  'Amb3')

sem2.5= sem(model2.5,R.prob2.5,60)
summary(sem2.5,digits=3)



##  If I follow the sem suggestion and set up model like the following
##  it will work but generate totally different results!
model2.5 <- specifyModel()
Ambit ->  Amb1,      a, NA,
Ambit -> Amb2 ,      b, NA,
Ambit -> Amb3 ,      c, NA,
Achieve -> Ach1,     d, NA,
Achieve -> Ach2,     e, NA,
Ambit <-> Achieve,   f, NA,
Amb1 <-> Amb1 ,      u, NA,
Amb2 <-> Amb2 ,      v, NA,
Amb3 <-> Amb3 ,      w, NA,
Ach1 <-> Ach1 ,      x, NA,
Ach2 <-> Ach2 ,      y, NA,
Achieve <-> Achieve,  NA, 1,
Ambit <-> Ambit,      NA, 1
##  If I follow the sem suggestion and set up model like the following
##  it will work but generate totally different results!


sem2.5= sem(model2.5, R.prob2.5, 60)
summary(sem2.5,digits=3)


#Fail here!


#Dr. Revelle's suggestion, 
colnames(R.prob2.5 ) <- c('Ach1',  'Ach2',  'Amb1',  'Amb2',  'Amb3')
sem2.5= sem(model2.5,R.prob2.5,60)
#Failed again,

sem2.5= sem(model2.5, R.prob2.5, 60, obs.var2.5)
#Failed here!


##  Never reaches here!
summary(sem2.5,digits=3)



