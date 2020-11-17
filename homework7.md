Homework7
================
Yamei Li, Meirou Guan, Tamires Amorim,Carol-Ann Jackson,

``` r
load("/Users/yameili/Desktop/ecob2000_lecture1/NHIS_2014.RData")
attach(data_use1)
max(AGE_P)
```

    ## [1] 85

``` r
use_varb <- (AGE_P >= 25) & (AGE_P <= 55)
dat_use <- subset(data_use1,use_varb) # 
detach()
attach(dat_use)
```

``` r
str(dat_use)
```

    ## 'data.frame':    46237 obs. of  36 variables:
    ##  $ NOTCOV             : num  1 0 0 1 1 0 0 0 0 0 ...
    ##  $ educ_nohs          : num  1 0 0 1 0 0 0 0 0 0 ...
    ##  $ educ_hs            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ educ_smcoll        : num  0 1 0 0 0 1 0 0 0 1 ...
    ##  $ educ_as            : num  0 0 0 0 0 0 1 0 1 0 ...
    ##  $ educ_bach          : num  0 0 1 0 0 0 0 1 0 0 ...
    ##  $ educ_adv           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ AGE_P              : num  41 25 47 51 49 25 32 36 46 42 ...
    ##  $ female             : num  1 0 1 1 0 0 0 1 0 1 ...
    ##  $ AfAm               : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ Asian              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ RaceOther          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Hispanic           : num  0 1 0 1 1 0 0 0 0 0 ...
    ##  $ Hispan_PR          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Hispan_Mex         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Hispan_DR          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ married            : num  0 0 1 1 1 0 1 1 1 1 ...
    ##  $ widowed            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ divorc_sep         : num  1 0 0 0 0 0 0 0 0 0 ...
    ##  $ REGION             : Factor w/ 4 levels "Northeast","Midwest",..: 3 4 4 3 3 3 4 4 2 2 ...
    ##  $ borninUSA          : num  1 1 1 0 0 1 1 1 1 1 ...
    ##  $ region_born        : Factor w/ 12 levels "US","Mex Cent Am Caribb",..: 1 1 1 2 2 1 1 1 1 1 ...
    ##  $ veteran_stat       : num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ inworkforce        : num  1 1 1 1 1 0 1 1 0 1 ...
    ##  $ ERNYR_P            : int  3 97 11 99 99 99 7 6 NA 6 ...
    ##  $ disabl_limit       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ person_healthstatus: Factor w/ 8 levels "Excellent","Very good",..: 2 1 2 3 4 1 3 3 4 3 ...
    ##  $ MEDICARE           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ MEDICAID           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ private_ins        : num  0 1 1 0 0 0 1 1 1 1 ...
    ##  $ RRP                : int  1 1 2 1 2 4 1 2 1 2 ...
    ##  $ HHX                : int  4 13 18 19 19 19 20 20 25 25 ...
    ##  $ FMX                : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ FPX                : int  1 1 2 1 2 3 1 2 1 3 ...
    ##  $ SCHIP              : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ sptn_medical       : Factor w/ 7 levels "zero","under 500",..: 3 2 3 3 3 3 6 6 3 3 ...

``` r
earn_lastyr <- as.factor(ERNYR_P)
str(earn_lastyr)
```

    ##  Factor w/ 14 levels "1","2","3","4",..: 3 12 11 14 14 14 7 6 NA 6 ...

``` r
levels(earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000 and over",NA,NA,NA)
summary(earn_lastyr)
```

    ##               0       $01-$4999     $5000-$9999   $10000-$14999   $15000-$19999 
    ##            1170            1363            2150            2151            2615 
    ##   $20000-$24999   $25000-$34999   $35000-$44999   $45000-$54999   $55000-$64999 
    ##            4615            3840            3002            2053            1563 
    ##   $65000-$74999 $75000 and over            NA's 
    ##            4617            4028           13070

Here we are looking into the insurance coverage as dependent variable,
and age, gender, race, education, marital status and region as the
explanatory variables for the insurance coverage.

``` r
model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born,
                    family = binomial, data = dat_use)
model_logit1
```

    ## 
    ## Call:  glm(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + 
    ##     RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
    ##     educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
    ##     REGION + region_born, family = binomial, data = dat_use)
    ## 
    ## Coefficients:
    ##                   (Intercept)                          AGE_P  
    ##                    -0.9711397                      0.0311287  
    ##                    I(AGE_P^2)                         female  
    ##                    -0.0007245                     -0.2987060  
    ##                          AfAm                          Asian  
    ##                    -0.1903598                     -0.1271264  
    ##                     RaceOther                       Hispanic  
    ##                     0.3640276                      0.2319377  
    ##                       educ_hs                    educ_smcoll  
    ##                    -0.2979181                     -0.6295006  
    ##                       educ_as                      educ_bach  
    ##                    -0.8456507                     -1.5671365  
    ##                      educ_adv                        married  
    ##                    -2.2067905                     -0.7303703  
    ##                       widowed                     divorc_sep  
    ##                    -0.0218213                     -0.1036226  
    ##                  veteran_stat                  REGIONMidwest  
    ##                    -0.5426507                      0.2835837  
    ##                   REGIONSouth                     REGIONWest  
    ##                     0.6903527                      0.2398669  
    ## region_bornMex Cent Am Caribb                region_bornS Am  
    ##                     1.1290736                      0.9776730  
    ##                region_bornEur         region_bornformer USSR  
    ##                     0.3557441                      0.8994373  
    ##             region_bornAfrica                region_bornMidE  
    ##                     0.7660519                      0.6970800  
    ##         region_bornIndia subc                region_bornAsia  
    ##                     0.6843085                      0.8439813  
    ##            region_bornSE Asia           region_bornElsewhere  
    ##                     0.3106985                      0.2215335  
    ##            region_bornunknown  
    ##                    -0.0311133  
    ## 
    ## Degrees of Freedom: 46236 Total (i.e. Null);  46206 Residual
    ## Null Deviance:       44990 
    ## Residual Deviance: 37760     AIC: 37820

For the simple linear regression (glm function) it is not necessary to
set the variables in a data frame. Although for the new models we are
using it will be necessary. Below is the data frame with the list of
variables set to have the same number of rows with unique row names in
the class “data.frame”. Because it includes a matrix it will force the
columns names to have unique results.

``` r
d_region <- data.frame(model.matrix(~ dat_use$REGION))
d_region_born <- data.frame(model.matrix(~ factor(dat_use$region_born)))  # snips any with zero in the subgroup
dat_for_analysis_sub <- data.frame(
  dat_use$NOTCOV,
  dat_use$AGE_P,
  dat_use$female,
  dat_use$AfAm,
  dat_use$Asian,
  dat_use$RaceOther,
  dat_use$Hispanic,
  dat_use$educ_hs,
  dat_use$educ_smcoll,
  dat_use$educ_as,
  dat_use$educ_bach,
  dat_use$educ_adv,
  dat_use$married,
  dat_use$widowed,
  dat_use$divorc_sep,
  d_region[,2:4],
  d_region_born[,2:12])
```

``` r
names(dat_for_analysis_sub) <- c("NOTCOV",
                                 "Age",
                                 "female",
                                 "AfAm",
                                 "Asian",
                                 "RaceOther",
                                 "Hispanic",
                                 "educ_hs",
                                 "educ_smcoll",
                                 "educ_as",
                                 "educ_bach",
                                 "educ_adv",
                                 "married",
                                 "widowed",
                                 "divorc_sep",
                                 "Region.Midwest",
                                 "Region.South",
                                 "Region.West",
                                 "born.Mex.CentAm.Carib",
                                 "born.S.Am",
                                 "born.Eur",
                                 "born.f.USSR",
                                 "born.Africa",
                                 "born.MidE",
                                 "born.India.subc",
                                 "born.Asia",
                                 "born.SE.Asia",
                                 "born.elsewhere",
                                 "born.unknown")
```

The goal here is to keep the regression parameters in a similar scale,
and ensure that the intercept represents the corrected mean, this way
the output of the regression will be easier to interpret.Following, we
split into training and test sets,

``` r
library(standardize)
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
# restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data
restrict_1 <- (runif(NN) < 0.1) # use 10% as training data
summary(restrict_1)
```

    ##    Mode   FALSE    TRUE 
    ## logical   41577    4660

``` r
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(NOTCOV ~ Age + female + AfAm + Asian + RaceOther + Hispanic + 
                      educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + 
                      married + widowed + divorc_sep + 
                      Region.Midwest + Region.South + Region.West + 
                      born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + 
                      born.Africa + born.MidE + born.India.subc + born.Asia + 
                      born.SE.Asia + born.elsewhere + born.unknown, dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)
```

The goal with this model is to understand how to call the standardized
objects from above and interpret the results with the “traditional
model” of linear regression. The lm function.

``` r
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
```

    ## 
    ## Call:
    ## lm(formula = sobj$formula, data = sobj$data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.66188 -0.21634 -0.10398  0.01318  1.07168 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             0.422272   0.127426   3.314 0.000927 ***
    ## Age                    -0.030873   0.005705  -5.412 6.56e-08 ***
    ## female1                -0.017212   0.005395  -3.190 0.001431 ** 
    ## AfAm1                  -0.022347   0.008468  -2.639 0.008345 ** 
    ## Asian1                 -0.017083   0.019300  -0.885 0.376116    
    ## RaceOther1              0.040207   0.020662   1.946 0.051723 .  
    ## Hispanic1               0.018641   0.009394   1.984 0.047278 *  
    ## educ_hs1               -0.046108   0.009031  -5.106 3.43e-07 ***
    ## educ_smcoll1           -0.067402   0.009973  -6.758 1.57e-11 ***
    ## educ_as1               -0.086807   0.010866  -7.989 1.71e-15 ***
    ## educ_bach1             -0.113737   0.009936 -11.447  < 2e-16 ***
    ## educ_adv1              -0.123109   0.011550 -10.659  < 2e-16 ***
    ## married1               -0.063156   0.006362  -9.928  < 2e-16 ***
    ## widowed1               -0.026419   0.029615  -0.892 0.372401    
    ## divorc_sep1            -0.027809   0.009959  -2.792 0.005252 ** 
    ## Region.Midwest1         0.016449   0.009180   1.792 0.073236 .  
    ## Region.South1           0.038740   0.008300   4.667 3.14e-06 ***
    ## Region.West1            0.017205   0.008657   1.987 0.046926 *  
    ## born.Mex.CentAm.Carib1  0.096729   0.011177   8.655  < 2e-16 ***
    ## born.S.Am1              0.074034   0.022634   3.271 0.001080 ** 
    ## born.Eur1               0.024291   0.023596   1.029 0.303319    
    ## born.f.USSR1            0.002329   0.052830   0.044 0.964834    
    ## born.Africa1            0.083412   0.028133   2.965 0.003043 ** 
    ## born.MidE1              0.101779   0.047250   2.154 0.031286 *  
    ## born.India.subc1        0.042028   0.027587   1.523 0.127709    
    ## born.Asia1              0.066993   0.028469   2.353 0.018656 *  
    ## born.SE.Asia1           0.020790   0.025659   0.810 0.417834    
    ## born.elsewhere1         0.033581   0.042116   0.797 0.425296    
    ## born.unknown1           0.009239   0.040184   0.230 0.818171    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3642 on 4631 degrees of freedom
    ## Multiple R-squared:  0.154,  Adjusted R-squared:  0.1488 
    ## F-statistic:  30.1 on 28 and 4631 DF,  p-value: < 2.2e-16

``` r
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
```

    ## Warning: contrasts dropped from factor female

    ## Warning: contrasts dropped from factor AfAm

    ## Warning: contrasts dropped from factor Asian

    ## Warning: contrasts dropped from factor RaceOther

    ## Warning: contrasts dropped from factor Hispanic

    ## Warning: contrasts dropped from factor educ_hs

    ## Warning: contrasts dropped from factor educ_smcoll

    ## Warning: contrasts dropped from factor educ_as

    ## Warning: contrasts dropped from factor educ_bach

    ## Warning: contrasts dropped from factor educ_adv

    ## Warning: contrasts dropped from factor married

    ## Warning: contrasts dropped from factor widowed

    ## Warning: contrasts dropped from factor divorc_sep

    ## Warning: contrasts dropped from factor Region.Midwest

    ## Warning: contrasts dropped from factor Region.South

    ## Warning: contrasts dropped from factor Region.West

    ## Warning: contrasts dropped from factor born.Mex.CentAm.Carib

    ## Warning: contrasts dropped from factor born.S.Am

    ## Warning: contrasts dropped from factor born.Eur

    ## Warning: contrasts dropped from factor born.f.USSR

    ## Warning: contrasts dropped from factor born.Africa

    ## Warning: contrasts dropped from factor born.MidE

    ## Warning: contrasts dropped from factor born.India.subc

    ## Warning: contrasts dropped from factor born.Asia

    ## Warning: contrasts dropped from factor born.SE.Asia

    ## Warning: contrasts dropped from factor born.elsewhere

    ## Warning: contrasts dropped from factor born.unknown

``` r
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
table(pred = pred_model_lpm1, true = dat_test$NOTCOV)
```

    ##        true
    ## pred        0     1
    ##   FALSE 32940  6631
    ##   TRUE    742  1264

``` r
summary(sobj$data)
```

    ##      NOTCOV              Age.V1        female   AfAm     Asian    RaceOther
    ##  Min.   :0.0000   Min.   :-1.6594215   1:2442   1: 647   1: 345   1:  81   
    ##  1st Qu.:0.0000   1st Qu.:-0.8874654   0:2218   0:4013   0:4315   0:4579   
    ##  Median :0.0000   Median :-0.0052300                                       
    ##  Mean   :0.1931   Mean   : 0.0000000                                       
    ##  3rd Qu.:0.0000   3rd Qu.: 0.8770055                                       
    ##  Max.   :1.0000   Max.   : 1.6489615                                       
    ##  Hispanic educ_hs  educ_smcoll educ_as  educ_bach educ_adv married  widowed 
    ##  1:1086   1:1177   1: 810      1: 556   1: 907    1: 483   1:2732   1:  40  
    ##  0:3574   0:3483   0:3850      0:4104   0:3753    0:4177   0:1928   0:4620  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  divorc_sep Region.Midwest Region.South Region.West born.Mex.CentAm.Carib
    ##  1: 486     1: 930         1:1610       1:1410      1: 640               
    ##  0:4174     0:3730         0:3050       0:3250      0:4020               
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##                                                                          
    ##  born.S.Am born.Eur born.f.USSR born.Africa born.MidE born.India.subc born.Asia
    ##  1:  73    1:  61   1:  12      1:  45      1:  15    1:  84          1:  67   
    ##  0:4587    0:4599   0:4648      0:4615      0:4645    0:4576          0:4593   
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##  born.SE.Asia born.elsewhere born.unknown
    ##  1: 112       1:  19         1:  21      
    ##  0:4548       0:4641         0:4639      
    ##                                          
    ##                                          
    ##                                          
    ## 

``` r
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
```

    ## 
    ## Call:
    ## glm(formula = sobj$formula, family = binomial, data = sobj$data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7191  -0.6384  -0.4313  -0.2489   2.8039  
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             0.40525    0.99791   0.406 0.684667    
    ## Age                    -0.23792    0.04396  -5.412 6.22e-08 ***
    ## female1                -0.13368    0.04111  -3.252 0.001146 ** 
    ## AfAm1                  -0.14737    0.06527  -2.258 0.023947 *  
    ## Asian1                 -0.12638    0.16263  -0.777 0.437104    
    ## RaceOther1              0.25223    0.13637   1.850 0.064375 .  
    ## Hispanic1               0.11788    0.06723   1.753 0.079563 .  
    ## educ_hs1               -0.22268    0.05646  -3.944 8.02e-05 ***
    ## educ_smcoll1           -0.36279    0.06620  -5.480 4.24e-08 ***
    ## educ_as1               -0.52301    0.07851  -6.662 2.71e-11 ***
    ## educ_bach1             -0.86811    0.08172 -10.623  < 2e-16 ***
    ## educ_adv1              -1.01967    0.11260  -9.055  < 2e-16 ***
    ## married1               -0.45731    0.04680  -9.771  < 2e-16 ***
    ## widowed1               -0.12055    0.21354  -0.565 0.572376    
    ## divorc_sep1            -0.16125    0.07048  -2.288 0.022139 *  
    ## Region.Midwest1         0.15064    0.07747   1.944 0.051836 .  
    ## Region.South1           0.33235    0.06819   4.874 1.09e-06 ***
    ## Region.West1            0.16831    0.07104   2.369 0.017824 *  
    ## born.Mex.CentAm.Carib1  0.52249    0.07367   7.092 1.32e-12 ***
    ## born.S.Am1              0.51719    0.14649   3.531 0.000415 ***
    ## born.Eur1               0.22500    0.19120   1.177 0.239283    
    ## born.f.USSR1            0.03391    0.53472   0.063 0.949435    
    ## born.Africa1            0.66323    0.19321   3.433 0.000598 ***
    ## born.MidE1              0.82261    0.31216   2.635 0.008409 ** 
    ## born.India.subc1        0.39818    0.23973   1.661 0.096725 .  
    ## born.Asia1              0.60207    0.22712   2.651 0.008028 ** 
    ## born.SE.Asia1           0.13979    0.22027   0.635 0.525663    
    ## born.elsewhere1         0.27672    0.30256   0.915 0.360414    
    ## born.unknown1           0.08349    0.27556   0.303 0.761903    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4573.6  on 4659  degrees of freedom
    ## Residual deviance: 3849.3  on 4631  degrees of freedom
    ## AIC: 3907.3
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
```

    ## Warning: contrasts dropped from factor female

    ## Warning: contrasts dropped from factor AfAm

    ## Warning: contrasts dropped from factor Asian

    ## Warning: contrasts dropped from factor RaceOther

    ## Warning: contrasts dropped from factor Hispanic

    ## Warning: contrasts dropped from factor educ_hs

    ## Warning: contrasts dropped from factor educ_smcoll

    ## Warning: contrasts dropped from factor educ_as

    ## Warning: contrasts dropped from factor educ_bach

    ## Warning: contrasts dropped from factor educ_adv

    ## Warning: contrasts dropped from factor married

    ## Warning: contrasts dropped from factor widowed

    ## Warning: contrasts dropped from factor divorc_sep

    ## Warning: contrasts dropped from factor Region.Midwest

    ## Warning: contrasts dropped from factor Region.South

    ## Warning: contrasts dropped from factor Region.West

    ## Warning: contrasts dropped from factor born.Mex.CentAm.Carib

    ## Warning: contrasts dropped from factor born.S.Am

    ## Warning: contrasts dropped from factor born.Eur

    ## Warning: contrasts dropped from factor born.f.USSR

    ## Warning: contrasts dropped from factor born.Africa

    ## Warning: contrasts dropped from factor born.MidE

    ## Warning: contrasts dropped from factor born.India.subc

    ## Warning: contrasts dropped from factor born.Asia

    ## Warning: contrasts dropped from factor born.SE.Asia

    ## Warning: contrasts dropped from factor born.elsewhere

    ## Warning: contrasts dropped from factor born.unknown

``` r
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$NOTCOV)
```

    ##        true
    ## pred        0     1
    ##   FALSE 32574  6342
    ##   TRUE   1108  1553

``` r
library(e1071)
# tuned_parameters <- tune.svm(as.factor(NOTCOV) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:1)) 
# summary(tuned_parameters)
# figure best parameters and input into next
svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$NOTCOV)
```

    ##     true
    ## pred     0     1
    ##    0 32008  6095
    ##    1  1674  1800

``` r
#LOSSO
x_varb <- cbind(AGE_P,I(AGE_P^2), female, AfAm, Asian , RaceOther, Hispanic, educ_hs, educ_smcoll, educ_as, educ_bach, educ_adv, married, widowed, divorc_sep, veteran_stat, REGION, region_born)
stand_Z <- function(X) {
rval <- matrix(data = NA, nrow = nrow(X), ncol = ncol(X))
for(j in 1:ncol(X)) rval[,j] <- (X[,j] - mean(X[,j]))/sd(X[,j])
return(rval)
}
x_varb_dm <- stand_Z(x_varb)
dimnames(x_varb_dm) <- dimnames(x_varb)
library(lars)
```

    ## Loaded lars 1.2

``` r
model_lars <- lars(x_varb_dm,NOTCOV)
summary(model_lars)
```

    ## LARS/LASSO
    ## Call: lars(x = x_varb_dm, y = NOTCOV)
    ##    Df    Rss       Cp
    ## 0   1 7122.1 7099.166
    ## 1   2 6816.6 4813.610
    ## 2   3 6795.4 4656.964
    ## 3   4 6748.7 4309.424
    ## 4   5 6579.4 3043.620
    ## 5   6 6452.3 2093.547
    ## 6   7 6437.8 1986.965
    ## 7   8 6405.4 1746.329
    ## 8   9 6404.6 1742.380
    ## 9  10 6332.2 1202.543
    ## 10 11 6288.2  874.627
    ## 11 12 6277.5  796.467
    ## 12 13 6255.1  630.785
    ## 13 14 6202.4  238.247
    ## 14 15 6183.0   94.986
    ## 15 16 6173.9   28.540
    ## 16 17 6173.6   28.868
    ## 17 18 6172.6   23.428
    ## 18 19 6171.8   19.000

``` r
plot(model_lars)
```

![](homework7_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
coef(model_lars)
```

    ##            AGE_P                   female          AfAm        Asian
    ##  [1,] 0.00000000  0.00000000  0.000000000  0.0000000000  0.000000000
    ##  [2,] 0.00000000  0.00000000  0.000000000  0.0000000000  0.000000000
    ##  [3,] 0.00000000  0.00000000  0.000000000  0.0000000000  0.000000000
    ##  [4,] 0.00000000  0.00000000  0.000000000  0.0000000000  0.000000000
    ##  [5,] 0.00000000  0.00000000  0.000000000  0.0000000000  0.000000000
    ##  [6,] 0.00000000 -0.01018644  0.000000000  0.0000000000  0.000000000
    ##  [7,] 0.00000000 -0.01129396  0.000000000  0.0000000000  0.000000000
    ##  [8,] 0.00000000 -0.01368239 -0.002377014  0.0000000000  0.000000000
    ##  [9,] 0.00000000 -0.01373286 -0.002443281  0.0000000000  0.000000000
    ## [10,] 0.00000000 -0.01796947 -0.006841564  0.0000000000  0.000000000
    ## [11,] 0.00000000 -0.02102429 -0.010018903  0.0000000000  0.000000000
    ## [12,] 0.00000000 -0.02185212 -0.010898409  0.0000000000  0.000000000
    ## [13,] 0.00000000 -0.02378664 -0.012961892  0.0000000000  0.000000000
    ## [14,] 0.00000000 -0.02705676 -0.016014383  0.0000000000  0.000000000
    ## [15,] 0.00000000 -0.02904647 -0.017764730  0.0000000000 -0.004994593
    ## [16,] 0.00000000 -0.03037608 -0.019208471  0.0000000000 -0.009618384
    ## [17,] 0.00000000 -0.03042707 -0.019272985  0.0000000000 -0.009843735
    ## [18,] 0.00000000 -0.03077801 -0.019682013 -0.0008481686 -0.011566567
    ## [19,] 0.04276532 -0.07326800 -0.019814676 -0.0012983769 -0.012138746
    ##         RaceOther   Hispanic     educ_hs  educ_smcoll      educ_as    educ_bach
    ##  [1,] 0.000000000 0.00000000  0.00000000  0.000000000  0.000000000  0.000000000
    ##  [2,] 0.000000000 0.04435295  0.00000000  0.000000000  0.000000000  0.000000000
    ##  [3,] 0.000000000 0.04659109  0.00000000  0.000000000  0.000000000  0.000000000
    ##  [4,] 0.000000000 0.05009762  0.00000000  0.000000000  0.000000000 -0.003234734
    ##  [5,] 0.000000000 0.05939686  0.00000000  0.000000000  0.000000000 -0.016277039
    ##  [6,] 0.000000000 0.06684947  0.00000000  0.000000000  0.000000000 -0.028680983
    ##  [7,] 0.000000000 0.06743863  0.00000000  0.000000000 -0.001676000 -0.030380252
    ##  [8,] 0.000000000 0.06875093  0.00000000  0.000000000 -0.005206883 -0.033963764
    ##  [9,] 0.000000000 0.06877618  0.00000000  0.000000000 -0.005284904 -0.034046053
    ## [10,] 0.000000000 0.06938024  0.00000000 -0.008887157 -0.013398328 -0.043092169
    ## [11,] 0.000000000 0.06989771  0.00000000 -0.015197937 -0.019178987 -0.049812480
    ## [12,] 0.000000000 0.06990894  0.00000000 -0.016980712 -0.020793839 -0.051681249
    ## [13,] 0.001637188 0.06991257  0.00000000 -0.021151447 -0.024560863 -0.055988226
    ## [14,] 0.003841745 0.06647341 -0.01894284 -0.038240832 -0.039616472 -0.074133320
    ## [15,] 0.004894220 0.06404933 -0.02947541 -0.047736795 -0.047991989 -0.084047291
    ## [16,] 0.005850864 0.06184896 -0.03908659 -0.056348403 -0.055586974 -0.093117736
    ## [17,] 0.005897424 0.06174113 -0.03955582 -0.056769039 -0.055957366 -0.093561127
    ## [18,] 0.006169301 0.06082307 -0.04287450 -0.059746433 -0.058613178 -0.096770450
    ## [19,] 0.006283881 0.06037939 -0.04393990 -0.060657713 -0.059514188 -0.097814982
    ##          educ_adv      married       widowed   divorc_sep  veteran_stat
    ##  [1,]  0.00000000  0.000000000  0.0000000000  0.000000000  0.0000000000
    ##  [2,]  0.00000000  0.000000000  0.0000000000  0.000000000  0.0000000000
    ##  [3,]  0.00000000 -0.002238138  0.0000000000  0.000000000  0.0000000000
    ##  [4,]  0.00000000 -0.005998940  0.0000000000  0.000000000  0.0000000000
    ##  [5,] -0.01300298 -0.016832838  0.0000000000  0.000000000  0.0000000000
    ##  [6,] -0.02491788 -0.024911514  0.0000000000  0.000000000  0.0000000000
    ##  [7,] -0.02648465 -0.025694842  0.0000000000  0.000000000  0.0000000000
    ##  [8,] -0.02979000 -0.027438959  0.0000000000  0.000000000  0.0000000000
    ##  [9,] -0.02986613 -0.027477121  0.0000000000  0.000000000 -0.0000643181
    ## [10,] -0.03780155 -0.030159431  0.0000000000  0.000000000 -0.0041223524
    ## [11,] -0.04383730 -0.032408941  0.0000000000  0.000000000 -0.0068552701
    ## [12,] -0.04550637 -0.033041208  0.0000000000  0.000000000 -0.0076332492
    ## [13,] -0.04936283 -0.034511654  0.0000000000  0.000000000 -0.0094656246
    ## [14,] -0.06429416 -0.036624124  0.0000000000  0.000000000 -0.0116194429
    ## [15,] -0.07244283 -0.037876982  0.0000000000  0.000000000 -0.0128758494
    ## [16,] -0.07989366 -0.040101591  0.0000000000 -0.002392354 -0.0139586267
    ## [17,] -0.08025778 -0.040229210 -0.0001000345 -0.002522531 -0.0140107743
    ## [18,] -0.08289057 -0.041265160 -0.0008143953 -0.003471549 -0.0143546467
    ## [19,] -0.08390851 -0.042496005 -0.0010603964 -0.004300069 -0.0144416020
    ##             REGION region_born
    ##  [1,] 0.0000000000 0.000000000
    ##  [2,] 0.0000000000 0.000000000
    ##  [3,] 0.0000000000 0.000000000
    ##  [4,] 0.0000000000 0.000000000
    ##  [5,] 0.0000000000 0.000000000
    ##  [6,] 0.0000000000 0.000000000
    ##  [7,] 0.0000000000 0.000000000
    ##  [8,] 0.0000000000 0.000000000
    ##  [9,] 0.0000000000 0.000000000
    ## [10,] 0.0000000000 0.000000000
    ## [11,] 0.0000000000 0.003442685
    ## [12,] 0.0007394185 0.004353788
    ## [13,] 0.0023545759 0.006491774
    ## [14,] 0.0046588468 0.008803641
    ## [15,] 0.0063210428 0.013482725
    ## [16,] 0.0078889340 0.017789990
    ## [17,] 0.0079651128 0.018001235
    ## [18,] 0.0084976405 0.019537791
    ## [19,] 0.0086700125 0.019958946

``` r
library('rpart')
# tree model 
modeltree <- rpart(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married+ widowed + divorc_sep + veteran_stat + REGION + region_born,data = dat_use, method = "class")
summary(modeltree)
```

    ## Call:
    ## rpart(formula = NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + 
    ##     Asian + RaceOther + Hispanic + educ_hs + educ_smcoll + educ_as + 
    ##     educ_bach + educ_adv + married + widowed + divorc_sep + veteran_stat + 
    ##     REGION + region_born, data = dat_use, method = "class")
    ##   n= 46237 
    ## 
    ##           CP nsplit rel error  xerror        xstd
    ## 1 0.02103468      0 1.0000000 1.00000 0.009595482
    ## 2 0.01000000      2 0.9579306 0.98556 0.009542092
    ## 
    ## Variable importance
    ## region_born    Hispanic       AGE_P  I(AGE_P^2)     married 
    ##          67          18           7           7           1 
    ## 
    ## Node number 1: 46237 observations,    complexity param=0.02103468
    ##   predicted class=0  expected loss=0.1902156  P(node) =1
    ##     class counts: 37442  8795
    ##    probabilities: 0.810 0.190 
    ##   left son=2 (39611 obs) right son=3 (6626 obs)
    ##   Primary splits:
    ##       region_born splits as  LRLLLLLLLLLL, improve=1127.1820, (0 missing)
    ##       Hispanic    < 0.5    to the left,    improve= 863.9143, (0 missing)
    ##       educ_bach   < 0.5    to the right,   improve= 299.5318, (0 missing)
    ##       married     < 0.5    to the right,   improve= 254.6254, (0 missing)
    ##       educ_adv    < 0.5    to the right,   improve= 252.9235, (0 missing)
    ##   Surrogate splits:
    ##       Hispanic < 0.5    to the left,  agree=0.894, adj=0.263, (0 split)
    ## 
    ## Node number 2: 39611 observations
    ##   predicted class=0  expected loss=0.1450607  P(node) =0.8566949
    ##     class counts: 33865  5746
    ##    probabilities: 0.855 0.145 
    ## 
    ## Node number 3: 6626 observations,    complexity param=0.02103468
    ##   predicted class=0  expected loss=0.460157  P(node) =0.1433051
    ##     class counts:  3577  3049
    ##    probabilities: 0.540 0.460 
    ##   left son=6 (3460 obs) right son=7 (3166 obs)
    ##   Primary splits:
    ##       AGE_P      < 39.5   to the right, improve=117.11560, (0 missing)
    ##       I(AGE_P^2) < 1560.5 to the right, improve=117.11560, (0 missing)
    ##       REGION     splits as  LRRL,       improve= 64.04262, (0 missing)
    ##       Hispanic   < 0.5    to the left,  improve= 61.63815, (0 missing)
    ##       AfAm       < 0.5    to the right, improve= 59.57061, (0 missing)
    ##   Surrogate splits:
    ##       I(AGE_P^2) < 1560.5 to the right, agree=1.000, adj=1.000, (0 split)
    ##       married    < 0.5    to the right, agree=0.556, adj=0.072, (0 split)
    ##       REGION     splits as  LRRL,       agree=0.532, adj=0.021, (0 split)
    ##       educ_hs    < 0.5    to the left,  agree=0.532, adj=0.021, (0 split)
    ##       RaceOther  < 0.5    to the left,  agree=0.523, adj=0.002, (0 split)
    ## 
    ## Node number 6: 3460 observations
    ##   predicted class=0  expected loss=0.3702312  P(node) =0.07483184
    ##     class counts:  2179  1281
    ##    probabilities: 0.630 0.370 
    ## 
    ## Node number 7: 3166 observations
    ##   predicted class=1  expected loss=0.4415666  P(node) =0.0684733
    ##     class counts:  1398  1768
    ##    probabilities: 0.442 0.558

``` r
post(modeltree, file = "tree_1.ps",
title = "Classification Tree for Health Insurance")
```

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 4.0-2

``` r
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV) 
# default is alpha = 1, lasso

par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 
```

![](homework7_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
plot(model1_elasticnet, xvar = "lambda")
```

![](homework7_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
plot(model1_elasticnet, xvar = "dev", label = TRUE)
```

![](homework7_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
print(model1_elasticnet)
```

    ## 
    ## Call:  glmnet(x = as.matrix(sobj$data[, -1]), y = sobj$data$NOTCOV) 
    ## 
    ##    Df  %Dev   Lambda
    ## 1   0  0.00 0.102500
    ## 2   1  1.14 0.093390
    ## 3   1  2.09 0.085090
    ## 4   1  2.88 0.077530
    ## 5   2  3.59 0.070640
    ## 6   2  4.24 0.064370
    ## 7   3  5.10 0.058650
    ## 8   3  5.96 0.053440
    ## 9   4  6.71 0.048690
    ## 10  4  7.44 0.044370
    ## 11  4  8.05 0.040420
    ## 12  5  8.61 0.036830
    ## 13  6  9.26 0.033560
    ## 14  6  9.82 0.030580
    ## 15  6 10.27 0.027860
    ## 16  6 10.65 0.025390
    ## 17  6 10.97 0.023130
    ## 18  7 11.24 0.021080
    ## 19  9 11.62 0.019200
    ## 20  9 11.94 0.017500
    ## 21  9 12.21 0.015940
    ## 22 10 12.44 0.014530
    ## 23 10 12.65 0.013240
    ## 24 11 12.88 0.012060
    ## 25 13 13.10 0.010990
    ## 26 14 13.30 0.010010
    ## 27 17 13.54 0.009124
    ## 28 17 13.75 0.008313
    ## 29 17 13.92 0.007575
    ## 30 17 14.06 0.006902
    ## 31 18 14.21 0.006289
    ## 32 18 14.38 0.005730
    ## 33 18 14.52 0.005221
    ## 34 19 14.65 0.004757
    ## 35 20 14.75 0.004335
    ## 36 22 14.84 0.003950
    ## 37 22 14.92 0.003599
    ## 38 22 14.99 0.003279
    ## 39 23 15.04 0.002988
    ## 40 24 15.10 0.002722
    ## 41 24 15.14 0.002480
    ## 42 24 15.18 0.002260
    ## 43 24 15.22 0.002059
    ## 44 24 15.24 0.001876
    ## 45 25 15.27 0.001710
    ## 46 25 15.29 0.001558
    ## 47 25 15.30 0.001419
    ## 48 25 15.31 0.001293
    ## 49 25 15.33 0.001178
    ## 50 26 15.33 0.001074
    ## 51 27 15.34 0.000978
    ## 52 27 15.35 0.000891
    ## 53 27 15.36 0.000812
    ## 54 27 15.37 0.000740
    ## 55 27 15.37 0.000674
    ## 56 27 15.37 0.000614
    ## 57 27 15.38 0.000560
    ## 58 27 15.38 0.000510
    ## 59 27 15.38 0.000465
    ## 60 27 15.39 0.000424
    ## 61 27 15.39 0.000386
    ## 62 27 15.39 0.000352
    ## 63 27 15.39 0.000320
    ## 64 27 15.39 0.000292
    ## 65 27 15.39 0.000266
    ## 66 27 15.39 0.000242
    ## 67 27 15.39 0.000221
    ## 68 27 15.39 0.000201
    ## 69 28 15.39 0.000183
    ## 70 28 15.39 0.000167
    ## 71 28 15.39 0.000152
    ## 72 28 15.39 0.000139
    ## 73 28 15.39 0.000126
    ## 74 28 15.39 0.000115
    ## 75 28 15.40 0.000105
    ## 76 28 15.40 0.000096

lm model: pred 0 1 FALSE 32940 6631 TRUE 742 1264 logit model: pred 0 1
FALSE 32574 6342 TRUE 1108 1553 svm model: pred 0 1 0 32008 6095 1 1674
1800 these three models, for the first one, the mis-classified is equal
to (6631+742)/(32940+6631+742+1264)=0.177. for the second one, the
mis-classified is equal to (6342+1108)/(32574+1553+6342+1108)=0.179 for
the third one, the mis-classified is equal to
(6095+1674)/(32008+1800+6095+1674)=0.187

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](homework7_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
