FinalProjectMath423
================
Dgebe Nicolas

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.1.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
## ---- load-the-dataset ----
file1<-paste0("https://raw.githubusercontent.com/",
"mcgillstat/regression/main/data/data-table-B3.csv")
data_table_B3 <- read.csv(file=file1)
# for demonstration, print the first six rows of the matrix
head(data_table_B3)
```

    ##    ï..y  x1  x2  x3   x4   x5 x6 x7    x8   x9  x10 x11
    ## 1 18.90 350 165 260 8.00 2.56  4  3 200.3 69.9 3910   1
    ## 2 17.00 350 170 275 8.50 2.56  4  3 199.6 72.9 3860   1
    ## 3 20.00 250 105 185 8.25 2.73  1  3 196.7 72.2 3510   1
    ## 4 18.25 351 143 255 8.00 3.00  2  3 199.9 74.0 3890   1
    ## 5 20.07 225  95 170 8.40 2.76  1  3 194.1 71.8 3365   0
    ## 6 11.20 440 215 330 8.20 2.88  4  3 184.5 69.0 4215   1

###1

``` r
#fit2 shows the MLR fit containing factors x1 and x6
fit1 <-lm(ï..y~x1, data=data_table_B3)
fit2 <-lm(ï..y~x1+x6, data=data_table_B3)
```

###2

``` r
#Let our H0:Beta6=0 and H1:Beta6=not equal 0
anova(fit1, fit2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: ï..y ~ x1
    ## Model 2: ï..y ~ x1 + x6
    ##   Res.Df    RSS Df Sum of Sq     F Pr(>F)
    ## 1     30 281.82                          
    ## 2     29 263.24  1     18.59 2.048 0.1631

#it appears that the p-value of our Anova test is 0.1631> alpha=0.05
thus we conclude that adding X6 is not significant and thus we cannot
reject out H0: B6=0 hypothesis

###3

``` r
#FIND R^2 and adj R^2 for our x1+x6 model
summary(fit2)
```

    ## 
    ## Call:
    ## lm(formula = ï..y ~ x1 + x6, data = data_table_B3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.0623 -1.6687 -0.3628  1.6221  6.2305 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 32.884551   1.535408  21.417  < 2e-16 ***
    ## x1          -0.053148   0.006137  -8.660 1.55e-09 ***
    ## x6           0.959223   0.670277   1.431    0.163    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.013 on 29 degrees of freedom
    ## Multiple R-squared:  0.7873, Adjusted R-squared:  0.7726 
    ## F-statistic: 53.67 on 2 and 29 DF,  p-value: 1.79e-10

#here we see that the R-squared: 0.7873 and Adjusted R-squared: 0.7726
for the model with x1+x6

``` r
#find R^2 and adj R^2 for out x1 model
summary(fit1)
```

    ## 
    ## Call:
    ## lm(formula = ï..y ~ x1, data = data_table_B3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.7923 -1.9752  0.0044  1.7677  6.8171 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 33.722677   1.443903   23.36  < 2e-16 ***
    ## x1          -0.047360   0.004695  -10.09 3.74e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.065 on 30 degrees of freedom
    ## Multiple R-squared:  0.7723, Adjusted R-squared:  0.7647 
    ## F-statistic: 101.7 on 1 and 30 DF,  p-value: 3.743e-11

# we see that for the model with only X1 the Multiple R-squared: 0.7723, Adjusted R-squared: 0.7647

#comparing fit 1 with fit 2 we see that R^2 for fit2 is higher then fit
1 but that is normal since it contains an extra parameter, also adjusted
R^2 is higher for fit2 and fit1 which suggest that fit2 is a more
appropriate model then fit 1

###4

``` r
#use summary to perform t-test for each predictor
fit9 <-lm(ï..y~x1+x2+x3+x4+x5+x6+x7+x8+x9, data=data_table_B3)
summary(fit9)
```

    ## 
    ## Call:
    ## lm(formula = ï..y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, 
    ##     data = data_table_B3)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -4.713 -1.655 -0.618  1.447  6.053 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) 30.78389   25.41040   1.211   0.2398  
    ## x1          -0.10310    0.04258  -2.422   0.0251 *
    ## x2          -0.07150    0.08503  -0.841   0.4104  
    ## x3           0.12081    0.08523   1.417   0.1718  
    ## x4           2.11104    2.92363   0.722   0.4786  
    ## x5           5.42868    3.00887   1.804   0.0863 .
    ## x6           0.53510    1.21437   0.441   0.6642  
    ## x7          -4.06305    2.75828  -1.473   0.1563  
    ## x8           0.09094    0.07628   1.192   0.2471  
    ## x9          -0.52174    0.28360  -1.840   0.0807 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.129 on 20 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.8281, Adjusted R-squared:  0.7507 
    ## F-statistic:  10.7 on 9 and 20 DF,  p-value: 6.821e-06

#after using t-test on each predictor we can see that based on the
corresponding p-values, predictors x2,x3,x4,x5,x6,x7,x8,x9 do not
contribute much to the model since their p-value is more then
alpha=0.05, and since the p-value for x1=0.0251 which is less then 0.05
it is signifcant to the model.

###5

``` r
#CI for beta1
confint(fit1, level = 0.95)
```

    ##                   2.5 %      97.5 %
    ## (Intercept) 30.77383383 36.67151954
    ## x1          -0.05694883 -0.03777032

###6

``` r
#calculate t-stat for H0:B1=0 and H0:B6=6 respectively

summary(fit2)$coefficients[2,3]
```

    ## [1] -8.660425

``` r
summary(fit2)$coefficients[3,3]
```

    ## [1] 1.431084

#if we look at the absolute values of the t-statistic for B1 =0 is
8.660425 and t-statistic for B6 = 0 is 1.431084. The t-statistic for B1
indicates that the coeffcient is much larger then its standard error
therefore it most likely different then 0 and for B6 the t-statistc
indicates that there is not a significant difference between coefficient
and standard error so x6 must not be a significant predictor

###7,8

``` r
#95% CI and PI for the model E[Y|X]=b0+b1x1+b6x6
predict(fit2,int="c",data.frame(x1=275,x6=2), level=0.95)
```

    ##        fit      lwr      upr
    ## 1 20.18739 18.87221 21.50257

``` r
predict(fit2,int="p",data.frame(x1=275,x6=2), level=0.95)
```

    ##        fit     lwr      upr
    ## 1 20.18739 13.8867 26.48808

###9

``` r
# residuals 
r <- resid(fit2)

# predictively adjusted residuals 7
pr <- resid(fit2)/(1 - lm.influence(fit2)$hat)

# SS_res -- residual sum of squares 10
ssres <- sum(r ^2) 


# PRESS statistic 
press <- sum(pr^2) 
```

##a) #Since PRESS≈(1+2γ)Σ(ŷi-yi) \> Σ(ŷi-yi)=SSres. Thus press will
always be larger then SSres

##b

``` r
#calculate sst and press
ssr <- sum((fitted(fit2) - mean(data_table_B3$ï..y))^2)
ssr
```

    ## [1] 974.3095

``` r
sst <- ssres+ssr
sst
```

    ## [1] 1237.544

``` r
Predicted_R <- (1-(press/sst))
Predicted_R
```

    ## [1] 0.7343405

#thus the out of sample R^2 =0.7343405 and sst=1237.544

##c #it is difficult to draw conclusions from just the predicted R^2
since adjusted R^2 would be a more accurate measurement. However, the
predicted R^2 seems close to 1 therefore this could be be a good
performing model

###10

``` r
dat = data_table_B3
# print the total number of observations
nrow(dat)
```

    ## [1] 32

``` r
## [1] 32
half_dat = dat[sample(nrow(dat), 16), ]
```

``` r
new_fit2 <-lm(ï..y~x1+x6, data=half_dat)
summary(new_fit2)
```

    ## 
    ## Call:
    ## lm(formula = ï..y ~ x1 + x6, data = half_dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.2491 -0.8394  0.2012  1.4535  5.5923 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 33.029215   2.175841  15.180 1.19e-09 ***
    ## x1          -0.061401   0.008779  -6.994 9.42e-06 ***
    ## x6           1.558000   0.974041   1.600    0.134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.056 on 13 degrees of freedom
    ## Multiple R-squared:  0.823,  Adjusted R-squared:  0.7958 
    ## F-statistic: 30.22 on 2 and 13 DF,  p-value: 1.294e-05

#there are no dramatic changes in the coeffcients, B0 and B1 are very
similar when compared to the full data, however B6=0.64944 for the
hald_data and B6=0.959223 for data_table_b3.The half data has an R^2
lower then the R^2 of the full data therefore we can conclude that it
does not predict as well as the full data since the more observations we
have the closer we get to the true regression and therefore the more
observations the better the prediction tends to be

###11

``` r
#fit all the models we want to comapre
fit0 <- lm(ï..y~1, data=data_table_B3)
fit1 <- lm(ï..y~x1, data=data_table_B3)
fit2 <- lm(ï..y~x6, data=data_table_B3)
fit3 <- lm(ï..y~x1+x6, data=data_table_B3)
n <- nrow(data_table_B3)
n
```

    ## [1] 32

``` r
#create a table with AIC BIC, R^2, ADJ R^2 and Cp
criteria.eval<-function(fit.obj,nv,bigsig.hat){
cvec<-rep(0,5)
SSRes<-sum(residuals(fit.obj)^2)
p<-length(coef(fit.obj))
cvec[1]<-summary(fit.obj)$r.squared
cvec[2]<-summary(fit.obj)$adj.r.squared
cvec[3]<-SSRes/bigsig.hat^2-n+2*p
cvec[4]<-AIC(fit.obj)

cvec[5]<-BIC(fit.obj)
return(cvec)
}
bigs.hat<-summary(fit3)$sigma
cvals<-matrix(0,nrow=4,ncol=5)
cvals[1,]<-criteria.eval(fit0,n,bigs.hat)
cvals[2,]<-criteria.eval(fit1,n,bigs.hat)
cvals[3,]<-criteria.eval(fit2,n,bigs.hat)
cvals[4,]<-criteria.eval(fit3,n,bigs.hat)

Criteria<-data.frame(cvals)
names(Criteria)<-c('Rsq','Adj.Rsq','Cp','AIC','BIC')
rownames(Criteria)<-c('1','x1','x6','x1+x6')
round(Criteria,4)
```

    ##          Rsq Adj.Rsq       Cp      AIC      BIC
    ## 1     0.0000  0.0000 106.3376 211.7768 214.7083
    ## x1    0.7723  0.7647   3.0480 166.4296 170.8268
    ## x6    0.2372  0.2117  76.0030 205.1139 209.5111
    ## x1+x6 0.7873  0.7726   3.0000 166.2460 172.1089

#R^2 should not be considered since it the more predictors there are the
higher it is, x1+x6 has the highest adj R^2, the lowest Cp and AIC
however x1 model has the lowest BIC. BIC should not hold as much weight
as AIC here because most likely the true regression is not in the list
of candidate models. Thus x1+x6 model seems to be the most appropriate
model with x1 being a close 2nd.
