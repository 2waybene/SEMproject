##  sem-example.R
##  http://personality-project.org/r/r.sem.html

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

sem2.5= sem(model2.5,  R.prob2.5, 60, obs.var2.5)


summary(sem2.5,digits=3)

Model Chisquare =  9.74   Df =  4 Pr(>Chisq) = 0.0450
Goodness-of-fit index =  0.964
Adjusted goodness-of-fit index =  0.865
RMSEA index =  0.120   90 % CI: (0.0164, 0.219)
BIC =  -15.1 

Normalized Residuals
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-5.77e-01 -3.78e-02 -2.04e-06  4.85e-03  3.87e-05  1.13e+00 

Parameter Estimates
Estimate Std Error z value Pr(>|z|)                   
a    0.920    0.0924   9.966 0.00e+00    Amb1 <--- Ambit
b    0.761    0.0955   7.974 1.55e-15    Amb2 <--- Ambit
c    0.652    0.0965   6.753 1.45e-11    Amb3 <--- Ambit
d    0.879    0.1762   4.986 6.16e-07  Ach1 <--- Achieve
e    0.683    0.1509   4.525 6.03e-06  Ach2 <--- Achieve
f    0.356    0.1138   3.127 1.76e-03 Achieve <--> Ambit
u    0.153    0.0982   1.557 1.20e-01     Amb1 <--> Amb1
v    0.420    0.0898   4.679 2.88e-06     Amb2 <--> Amb2
w    0.575    0.0949   6.061 1.35e-09     Amb3 <--> Amb3
x    0.228    0.2791   0.816 4.15e-01     Ach1 <--> Ach1
y    0.534    0.1837   2.905 3.67e-03     Ach2 <--> Ach2

Iterations =  26 
>  #causal structure with errors in achievement
  >   #ambition -> achievement
  >   
  >  model2.51=matrix(c(
    + 	'Ambit ->  Amb1',      'a', NA,
    + 	'Ambit -> Amb2' ,      'b', NA,
    + 	'Ambit -> Amb3' ,      'c', NA,
    + 	'Achieve -> Ach1',     'd', NA,
    + 	'Achieve -> Ach2',     'e', NA,
    + 	'Ambit -> Achieve',   'f', NA,
    + 	'Amb1 <-> Amb1' ,      'u', NA,
    + 	'Amb2 <-> Amb2' ,      'v', NA,
    + 	'Amb3 <-> Amb3' ,      'w', NA,
    + 	'Ach1 <-> Ach1' ,      'x', NA,
    + 	'Ach2 <-> Ach2' ,      'y', NA,
    + 	'Achieve <-> Achieve',  NA, 1,
    + 	'Ambit <-> Ambit',      NA, 1),
    + 	ncol=3, byrow=TRUE)




> 	
  >  sem2.51= sem(model2.51,R.prob2.5,100, obs.var2.5)
>  summary(sem2.51,digits=3)

Model Chisquare =  9.74   Df =  4 Pr(>Chisq) = 0.0450
Goodness-of-fit index =  0.964
Adjusted goodness-of-fit index =  0.865
RMSEA index =  0.120   90 % CI: (0.0164, 0.219)
BIC =  -15.1 

Normalized Residuals
Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-5.77e-01 -3.78e-02 -6.33e-06  4.85e-03  3.06e-05  1.13e+00 

Parameter Estimates
Estimate Std Error z value Pr(>|z|)                   
a    0.920    0.0924   9.966 0.00e+00    Amb1 <--- Ambit
b    0.761    0.0955   7.974 1.55e-15    Amb2 <--- Ambit
c    0.652    0.0965   6.753 1.45e-11    Amb3 <--- Ambit
d    0.821    0.1781   4.613 3.98e-06  Ach1 <--- Achieve
e    0.638    0.1343   4.752 2.01e-06  Ach2 <--- Achieve
f    0.381    0.1394   2.731 6.32e-03 Achieve <--- Ambit
u    0.153    0.0982   1.557 1.20e-01     Amb1 <--> Amb1
v    0.420    0.0898   4.679 2.88e-06     Amb2 <--> Amb2
w    0.575    0.0949   6.061 1.35e-09     Amb3 <--> Amb3
x    0.228    0.2791   0.816 4.15e-01     Ach1 <--> Ach1
y    0.534    0.1837   2.906 3.67e-03     Ach2 <--> Ach2

Iterations =  27 
> #causal structure with errors in achievement
  >   #ambition -> achievement
  >   
  >  model2.52=matrix(c(
    + 	'Ambit ->  Amb1',      'a', NA,
    + 	'Ambit -> Amb2' ,      'b', NA,
    + 	'Ambit -> Amb3' ,      'c', NA,
    + 	'Achieve -> Ach1',     'd', NA,
    + 	'Achieve -> Ach2',     'e', NA,
    + 	'Ambit -> Achieve',   'f', NA,
    + 	'Amb1 <-> Amb1' ,      'u', NA,
    + 	'Amb2 <-> Amb2' ,      'v', NA,
    + 	'Amb3 <-> Amb3' ,      'w', NA,
    + 	'Ach1 <-> Ach1' ,      'x', NA,
    + 	'Ach2 <-> Ach2' ,      'y', NA,
    + 	'Achieve <-> Achieve', 'z', NA,
    + 	'Ambit <-> Ambit',      NA, 1),
    + 	ncol=3, byrow=TRUE)
> 	
  >  sem2.52= sem(model2.52,R.prob2.5,100, obs.var2.5)
Warning message: 
  Negative parameter variances.
Model is probably underidentified.
in: sem.default(ram = ram, S = S, N = N, param.names = pars, var.names = vars,  
                >  summary(sem2.52,digits=3)
                
                Model Chisquare =  9.74   Df =  3 Pr(>Chisq) = 0.0209
                Goodness-of-fit index =  0.964
                Adjusted goodness-of-fit index =  0.82
                RMSEA index =  0.151   90 % CI: (0.0517, 0.261)
                BIC =  -8.9 
                
                Normalized Residuals
                Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
                -5.77e-01 -3.78e-02 -1.62e-06  4.85e-03  3.43e-05  1.13e+00 
                
                Parameter Estimates
                Estimate Std Error z value Pr(>|z|)                     
                a    0.920    0.0924   9.966 0.00e+00      Amb1 <--- Ambit
                b    0.761    0.0955   7.974 1.55e-15      Amb2 <--- Ambit
                c    0.652    0.0965   6.753 1.45e-11      Amb3 <--- Ambit
                d    0.905       NaN     NaN      NaN    Ach1 <--- Achieve
                e    0.703       NaN     NaN      NaN    Ach2 <--- Achieve
                f    0.346       NaN     NaN      NaN   Achieve <--- Ambit
                u    0.153    0.0982   1.557 1.20e-01       Amb1 <--> Amb1
                v    0.420    0.0898   4.679 2.88e-06       Amb2 <--> Amb2
                w    0.575    0.0949   6.061 1.35e-09       Amb3 <--> Amb3
                x    0.228    0.2791   0.816 4.15e-01       Ach1 <--> Ach1
                y    0.534    0.1837   2.905 3.67e-03       Ach2 <--> Ach2
                z    0.824       NaN     NaN      NaN Achieve <--> Achieve
                
                Iterations =  27 
                
                Aliased parameters: d e f z 
                Warning message: 
                  NaNs produced in: sqrt(diag(object$cov)) 
                >   #causal structure with errors in achievement
                  >   #ambition -> achievement
                  >   #fix achievement to Loehlin answer
                  >   
                  >  model2.53=matrix(c(
                    + 	'Ambit ->  Amb1',      'a', NA,
                    + 	'Ambit -> Amb2' ,      'b', NA,
                    + 	'Ambit -> Amb3' ,      'c', NA,
                    + 	'Achieve -> Ach1',     'd', NA,
                    + 	'Achieve -> Ach2',     'e', NA,
                    + 	'Ambit -> Achieve',   'f', NA,
                    + 	'Amb1 <-> Amb1' ,      'u', NA,
                    + 	'Amb2 <-> Amb2' ,      'v', NA,
                    + 	'Amb3 <-> Amb3' ,      'w', NA,
                    + 	'Ach1 <-> Ach1' ,      'x', NA,
                    + 	'Ach2 <-> Ach2' ,      'y', NA,
                    + 	'Achieve <-> Achieve', NA, .873,
                    + 	'Ambit <-> Ambit',      NA, 1),
                    + 	ncol=3, byrow=TRUE)
                > 	
                  >  sem2.53= sem(model2.53,R.prob2.5,100, obs.var2.5)
                >  summary(sem2.53,digits=3)\
                Error: syntax error
                > summary(sem2.53,digits=3)
                
                Model Chisquare =  9.74   Df =  4 Pr(>Chisq) = 0.0450
                Goodness-of-fit index =  0.964
                Adjusted goodness-of-fit index =  0.865
                RMSEA index =  0.120   90 % CI: (0.0164, 0.219)
                BIC =  -15.1 
                
                Normalized Residuals
                Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
                -5.77e-01 -3.78e-02 -1.14e-06  4.85e-03  3.19e-05  1.13e+00 
                
                Parameter Estimates
                Estimate Std Error z value Pr(>|z|)                   
                a    0.920    0.0924   9.966 0.00e+00    Amb1 <--- Ambit
                b    0.761    0.0955   7.974 1.55e-15    Amb2 <--- Ambit
                c    0.652    0.0965   6.753 1.45e-11    Amb3 <--- Ambit
                d    0.879    0.1906   4.612 3.98e-06  Ach1 <--- Achieve
                e    0.683    0.1437   4.752 2.01e-06  Ach2 <--- Achieve
                f    0.356    0.1303   2.731 6.32e-03 Achieve <--- Ambit
                u    0.153    0.0982   1.557 1.20e-01     Amb1 <--> Amb1
                v    0.420    0.0898   4.679 2.88e-06     Amb2 <--> Amb2
                w    0.575    0.0949   6.061 1.35e-09     Amb3 <--> Amb3
                x    0.228    0.2791   0.816 4.15e-01     Ach1 <--> Ach1
                y    0.534    0.1837   2.906 3.67e-03     Ach2 <--> Ach2
                
                Iterations =  27 
                > 
                  
                  
                  컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
                
                #Loehlin problem from table 2-12
                #Note that version a is a classic example of congeneric measurement.
                #Alternatively, this could be thought of as underidentified higher order model
                
                obs.var2.12 = c('a',  'b',  'c',  'd')
                R.prob2.12 = matrix(c(
                  1.00 ,  .30,    .20,   .10,                                               
                  .30,  1.00,   .20,   .20,                                               
                  .20,   .20,  1.00,   .30,                                            
                  .10,   .20,   .30,  1.00), 
                  ncol=4,byrow=TRUE)
                
                model2.12a=matrix(c(
                  'g ->  a',      'a1', NA,
                  'g -> b' ,      'b1', NA,
                  'g -> c' ,      'c1', NA,
                  'g -> d',     'd1', NA,
                  'a <-> a',      'e1', NA,
                  'b <-> b',      'e2', NA,
                  'c <-> c',      'e3', NA,
                  'd <-> d',      'e4', NA,
                  'g <-> g',       NA, 1),
                  ncol=3, byrow=TRUE)
                sem2.12a= sem(model2.12a,R.prob2.12,120, obs.var2.12)
                summary(sem2.12a,digits=3)
                
                #a 1 degree of freedom model
                model2.12b=matrix(c(
                  'g ->  a',      'a1', NA,
                  'g -> b' ,      'b1', NA,
                  'f -> c' ,      'c1', NA,
                  'f -> d',     'd1', NA,
                  'a <-> a',      'e1', NA,
                  'b <-> b',      'e2', NA,
                  'c <-> c',      'e3', NA,
                  'd <-> d',      'e4', NA,
                  'g <-> g',       NA, 1,
                  'f <-> f',       NA, 1,
                  'g <-> f',        'fg', NA),
                  ncol=3, byrow=TRUE)
                sem2.12b= sem(model2.12b,R.prob2.12,120, obs.var2.12)
                summary(sem2.12b,digits=3)
                
                #the following higher level model has 0 degrees of freedom
                model2.12c=matrix(c(
                  'g ->  a',      'a1', NA,
                  'g -> b' ,      'b1', NA,
                  'f -> c' ,      'c1', NA,
                  'f -> d',       'd1', NA,
                  'a <-> a',      'e1', NA,
                  'b <-> b',      'e2', NA,
                  'c <-> c',      'e3', NA,
                  'd <-> d',      'e4', NA,
                  'g <-> g',       NA, 1,
                  'f <-> f',       NA, 1,
                  'h -> g',        'hg', NA,
                  'h -> f',        NA,1,
                  'h <-> h',        NA,1),
                  ncol=3, byrow=TRUE)
                sem2.12c= sem(model2.12c,R.prob2.12,120, obs.var2.12)
                summary(sem2.12c,digits=3)
                
                
                
                컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
                
                #Loehlin problem 2.9
                
                obs.var2.09 = c('w',  'x',  'y',  'z')
                R.prob2.09 = matrix(c(
                  1.00 ,  .40,    .50,   .30,                                               
                  .40,  1.00,   .55,   .35,                                               
                  .50,   .55,  1.00,   .40,                                            
                  .30,   .35,   .40,  1.00), 
                  ncol=4,byrow=TRUE)
                
                model2.09=matrix(c(
                  'g ->  w',      'a1', NA,
                  'g -> x' ,      'b1', NA,
                  'g -> y' ,      'c1', NA,
                  'g -> z',     'd1', NA,
                  'w <-> w',      'e1', NA,
                  'x <-> x',      'e2', NA,
                  'y <-> y',      'e3', NA,
                  'z <-> z',      'e4', NA,
                  'g <-> g',       NA, 1),
                  ncol=3, byrow=TRUE)
                sem2.09= sem(model2.09,R.prob2.09,500, obs.var2.09)
                summary(sem2.09,digits=3)
                
                
                obs.var2.09b = c('w',  'x',  'y',  'z')
                
                R.prob2.09b = matrix(c(
                  1.00 ,  .40,    .50,   .30,                                               
                  .40,  1.00,   .55,   .35,                                               
                  .50,   .55,  1.00,   .40,                                            
                  .30,   .35,   .40,  1.00), 
                  ncol=4,byrow=TRUE)
                
                model2.09b=matrix(c(
                  'g ->  w',      NA,1,
                  'g -> x' ,      'b1', NA,
                  'g -> y' ,      'c1', NA,
                  'g -> z',     'd1', NA,
                  'w <-> w',      'e1', NA,
                  'x <-> x',      'e2', NA,
                  'y <-> y',      'e3', NA,
                  'z <-> z',      'e4', NA,
                  'g <-> g',       'e',NA),
                  ncol=3, byrow=TRUE)
                
                sem2.09b= sem(model2.09b,R.prob2.09b,500, obs.var2.09b)
                summary(sem2.09b,digits=3)
                
                
                part of a short guide to R 
                Version of February 15, 2007 
                William Revelle Department of Psychology 
                Northwestern University
                Useful R links
                Readings and software:
                  Comprehensive R Archive Network (CRAN)
                An introduction to R
                R Studio
                Structural Equation modelling:
                  sem
                lavaan
                psych for sem
                EFA and factor extension (fa)
                Multilevel modeling:
                  Multilevel
                Linear and Non Linear Mixed Effects nlme
                statsBy
                Item Response Models:
                  Latent Trait Model (ltm)
                mirt
                mokken
                irt by factor analysis (irt.fa)
                More on the psych package
                The psych package is a work in progress. The current released version is 1.3.2. Updates are added sporadically, but usually at least once a quarter.	The development version is always available at the pmc repository.
                
                If you want to help us develop our understanding of personality, please take our test at SAPA Project.
                
                쯜illiam Revelle and the Personality Project. All rights reserved. 
                As is true of all webpages, this is a work in progress. 
                Design: HTML5 Up! | Modified by Jason A. French and William Revelle 
                Version of May 20, 2013