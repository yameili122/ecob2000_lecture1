---
title: "homework1"
author: "Yamei Li" 
date: "9/12/2020"
output: html_document
---
question1:
my group member are: Meirou Guan and Tamires Amorim

question 2:
I rolled a dice 20 times, what the result was:
 5,1,5,2,3, 1,1,4,6,4,
 5,4,2,4,6, 2,1,5,3,1.
 2 out of 20 times I rolled 6. In fact, it is more less than I expect, because I think the result should be 3 or 4.
I wrote some code in R to simulate a fair roll


```r
dice<-c(1,2,3,4,5,6)
sample(dice, size=20, replace=TRUE)
```

```
##  [1] 5 6 2 6 3 1 1 4 5 3 2 1 6 4 6 2 4 1 1 6
```

```r
sample
```

```
## function (x, size, replace = FALSE, prob = NULL) 
## {
##     if (length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 
##         1) {
##         if (missing(size)) 
##             size <- x
##         sample.int(x, size, replace, prob)
##     }
##     else {
##         if (missing(size)) 
##             size <- length(x)
##         x[sample.int(length(x), size, replace, prob)]
##     }
## }
## <bytecode: 0x7fe01b741120>
## <environment: namespace:base>
```

 we can get 3 out of 20 times rolled 6,according to the probability it is a fair roll,if we want to get more often 6, we can write some code,like:

```r
dice<-c(2,3,4,5,6)
sample(dice, size=20, replace=TRUE)
```

```
##  [1] 6 4 3 5 4 5 6 6 3 3 4 2 3 2 4 5 6 4 4 2
```

```r
sample
```

```
## function (x, size, replace = FALSE, prob = NULL) 
## {
##     if (length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 
##         1) {
##         if (missing(size)) 
##             size <- x
##         sample.int(x, size, replace, prob)
##     }
##     else {
##         if (missing(size)) 
##             size <- length(x)
##         x[sample.int(length(x), size, replace, prob)]
##     }
## }
## <bytecode: 0x7fe01b741120>
## <environment: namespace:base>
```

```r
roll_die = function(n) sample(1:6, n, rep = T,prob = c(rep(1/7, 5), 2/7))
roll_die(20)
```

```
##  [1] 1 1 6 5 2 6 3 5 1 6 2 1 3 2 6 6 4 6 4 4
```
The first function is a simulation of a fair roll, where we rolled the die 20 times and the number 6 appeared three times. On the second function we manipulate the numbers to create the event “6” more often than on a fair roll. In the real world a die or a pair of dice, can be manipulated by drilling, sanding or heating to weight one side of the dice, this is the only way to increase the chances of obtaining the same number more than expected. Otherwise, since the events are independent within a sample the probability of a 6 to come will always be 1/6, no matter the amount of rolls.

question 4:
Practice R Basics for lecture 1.

```r
load("acs2017_ny_data.RData")
#glimpse(acs2017_ny) try this later
acs2017_ny[1:10,1:7]
```

```
##    AGE female educ_nohs educ_hs educ_somecoll educ_college educ_advdeg
## 1   72      1         0       0             0            0           1
## 2   72      0         0       0             0            0           1
## 3   31      0         0       0             0            1           0
## 4   28      1         0       0             0            1           0
## 5   54      0         0       0             0            0           1
## 6   45      1         0       1             0            0           0
## 7   84      1         0       0             1            0           0
## 8   71      0         0       0             0            1           0
## 9   68      1         0       0             1            0           0
## 10  37      1         1       0             0            0           0
```

```r
summary(acs2017_ny)
```

```
##       AGE            female         educ_nohs        educ_hs      
##  Min.   : 0.00   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
##  1st Qu.:22.00   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000  
##  Median :42.00   Median :1.0000   Median :0.000   Median :0.0000  
##  Mean   :41.57   Mean   :0.5156   Mean   :0.271   Mean   :0.2804  
##  3rd Qu.:60.00   3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1.0000  
##  Max.   :95.00   Max.   :1.0000   Max.   :1.000   Max.   :1.0000  
##                                                                   
##  educ_somecoll    educ_college     educ_advdeg                  SCHOOL      
##  Min.   :0.000   Min.   :0.0000   Min.   :0.000   N/A              :  5569  
##  1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.000   No, not in school:144968  
##  Median :0.000   Median :0.0000   Median :0.000   Yes, in school   : 46048  
##  Mean   :0.173   Mean   :0.1567   Mean   :0.119   Missing          :     0  
##  3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.000                             
##  Max.   :1.000   Max.   :1.0000   Max.   :1.000                             
##                                                                             
##                         EDUC      
##  Grade 12                 :55119  
##  4 years of college       :30802  
##  5+ years of college      :23385  
##  1 year of college        :19947  
##  Nursery school to grade 4:14240  
##  2 years of college       :14065  
##  (Other)                  :39027  
##                                           EDUCD      
##  Regular high school diploma                 :35689  
##  Bachelor's degree                           :30802  
##  1 or more years of college credit, no degree:19947  
##  Master's degree                             :17010  
##  Associate's degree, type not specified      :14065  
##  Some college, but less than 1 year          : 9086  
##  (Other)                                     :69986  
##                                      DEGFIELD     
##  N/A                                     :142398  
##  Business                                :  9802  
##  Education Administration and Teaching   :  6708  
##  Social Sciences                         :  4836  
##  Medical and Health Sciences and Services:  3919  
##  Fine Arts                               :  3491  
##  (Other)                                 : 25431  
##                                   DEGFIELDD     
##  N/A                                   :142398  
##  Psychology                            :  2926  
##  Business Management and Administration:  2501  
##  Accounting                            :  2284  
##  General Education                     :  2238  
##  English Language and Literature       :  2202  
##  (Other)                               : 42036  
##                                  DEGFIELD2     
##  N/A                                  :190425  
##  Business                             :   972  
##  Social Sciences                      :   853  
##  Education Administration and Teaching:   611  
##  Fine Arts                            :   465  
##  Communications                       :   352  
##  (Other)                              :  2907  
##                                                            DEGFIELD2D    
##  N/A                                                            :190425  
##  Psychology                                                     :   284  
##  Economics                                                      :   260  
##  Political Science and Government                               :   243  
##  Business Management and Administration                         :   217  
##  French, German, Latin and Other Common Foreign Language Studies:   205  
##  (Other)                                                        :  4951  
##       PUMA            GQ           OWNERSHP       OWNERSHPD        MORTGAGE    
##  Min.   : 100   Min.   :1.000   Min.   :0.000   Min.   : 0.00   Min.   :0.000  
##  1st Qu.:1500   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:12.00   1st Qu.:0.000  
##  Median :3201   Median :1.000   Median :1.000   Median :13.00   Median :1.000  
##  Mean   :2713   Mean   :1.148   Mean   :1.266   Mean   :14.95   Mean   :1.453  
##  3rd Qu.:3902   3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.:22.00   3rd Qu.:3.000  
##  Max.   :4114   Max.   :5.000   Max.   :2.000   Max.   :22.00   Max.   :4.000  
##                                                                                
##     OWNCOST           RENT         COSTELEC       COSTGAS        COSTWATR   
##  Min.   :    0   Min.   :   0   Min.   :   0   Min.   :   0   Min.   :   0  
##  1st Qu.: 1208   1st Qu.:   0   1st Qu.: 960   1st Qu.: 840   1st Qu.: 320  
##  Median : 2891   Median :   0   Median :1560   Median :2400   Median :1400  
##  Mean   :38582   Mean   : 393   Mean   :2311   Mean   :5032   Mean   :4836  
##  3rd Qu.:99999   3rd Qu.: 630   3rd Qu.:2520   3rd Qu.:9993   3rd Qu.:9993  
##  Max.   :99999   Max.   :3800   Max.   :9997   Max.   :9997   Max.   :9997  
##                                                                             
##     COSTFUEL       HHINCOME          FOODSTMP        LINGISOL    
##  Min.   :   0   Min.   : -11800   Min.   :1.000   Min.   :0.000  
##  1st Qu.:9993   1st Qu.:  41600   1st Qu.:1.000   1st Qu.:1.000  
##  Median :9993   Median :  81700   Median :1.000   Median :1.000  
##  Mean   :7935   Mean   : 114902   Mean   :1.147   Mean   :1.002  
##  3rd Qu.:9993   3rd Qu.: 140900   3rd Qu.:1.000   3rd Qu.:1.000  
##  Max.   :9997   Max.   :2030000   Max.   :2.000   Max.   :2.000  
##                 NA's   :10630                                    
##      ROOMS           BUILTYR2         UNITSSTR        FUELHEAT    
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.00   Min.   :0.000  
##  1st Qu.: 4.000   1st Qu.: 1.000   1st Qu.: 3.00   1st Qu.:2.000  
##  Median : 6.000   Median : 3.000   Median : 3.00   Median :2.000  
##  Mean   : 5.887   Mean   : 3.711   Mean   : 4.39   Mean   :2.959  
##  3rd Qu.: 8.000   3rd Qu.: 5.000   3rd Qu.: 6.00   3rd Qu.:4.000  
##  Max.   :16.000   Max.   :22.000   Max.   :10.00   Max.   :9.000  
##                                                                   
##       SSMC            FAMSIZE           NCHILD           NCHLT5       
##  Min.   :0.00000   Min.   : 1.000   Min.   :0.0000   Min.   :0.00000  
##  1st Qu.:0.00000   1st Qu.: 2.000   1st Qu.:0.0000   1st Qu.:0.00000  
##  Median :0.00000   Median : 3.000   Median :0.0000   Median :0.00000  
##  Mean   :0.01102   Mean   : 3.087   Mean   :0.5009   Mean   :0.08441  
##  3rd Qu.:0.00000   3rd Qu.: 4.000   3rd Qu.:1.0000   3rd Qu.:0.00000  
##  Max.   :2.00000   Max.   :19.000   Max.   :9.0000   Max.   :5.00000  
##                                                                       
##      RELATE          RELATED           MARST            RACE          RACED    
##  Min.   : 1.000   Min.   : 101.0   Min.   :1.000   Min.   :1.00   Min.   :100  
##  1st Qu.: 1.000   1st Qu.: 101.0   1st Qu.:1.000   1st Qu.:1.00   1st Qu.:100  
##  Median : 2.000   Median : 201.0   Median :5.000   Median :1.00   Median :100  
##  Mean   : 3.307   Mean   : 335.6   Mean   :3.742   Mean   :2.03   Mean   :205  
##  3rd Qu.: 3.000   3rd Qu.: 301.0   3rd Qu.:6.000   3rd Qu.:2.00   3rd Qu.:200  
##  Max.   :13.000   Max.   :1301.0   Max.   :6.000   Max.   :9.00   Max.   :990  
##                                                                                
##      HISPAN          HISPAND                  BPL        
##  Min.   :0.0000   Min.   :  0.00   New York     :128517  
##  1st Qu.:0.0000   1st Qu.:  0.00   West Indies  :  8481  
##  Median :0.0000   Median :  0.00   China        :  4964  
##  Mean   :0.4153   Mean   : 44.75   SOUTH AMERICA:  4957  
##  3rd Qu.:0.0000   3rd Qu.:  0.00   India        :  3476  
##  Max.   :4.0000   Max.   :498.00   Pennsylvania :  3303  
##                                    (Other)      : 42887  
##                  BPLD                            ANCESTR1    
##  New York          :128517   Not Reported            :32021  
##  China             :  4116   Italian                 :20577  
##  Dominican Republic:  3517   Irish, various subheads,:16388  
##  Pennsylvania      :  3303   German                  :12781  
##  New Jersey        :  3127   African-American        : 9559  
##  Puerto Rico       :  2272   United States           : 8209  
##  (Other)           : 51733   (Other)                 :97050  
##                                    ANCESTR1D             ANCESTR2     
##  Not Reported                           :32021   Not Reported:141487  
##  Italian (1990-2000, ACS, PRCS)         :20577   German      :  9476  
##  Irish                                  :15651   Irish       :  9238  
##  German (1990-2000, ACS/PRCS)           :12605   English     :  4895  
##  African-American (1990-2000, ACS, PRCS): 9559   Italian     :  4531  
##  United States                          : 8209   Polish      :  3113  
##  (Other)                                :97963   (Other)     : 23845  
##                           ANCESTR2D         CITIZEN          YRSUSA1      
##  Not Reported                  :141487   Min.   :0.0000   Min.   : 0.000  
##  German (1990-2000, ACS, PRCS) :  9441   1st Qu.:0.0000   1st Qu.: 0.000  
##  Irish                         :  8809   Median :0.0000   Median : 0.000  
##  English                       :  4895   Mean   :0.4793   Mean   : 5.377  
##  Italian (1990-2000, ACS, PRCS):  4531   3rd Qu.:0.0000   3rd Qu.: 0.000  
##  Polish                        :  3113   Max.   :3.0000   Max.   :92.000  
##  (Other)                       : 24309                                    
##     HCOVANY         HCOVPRIV         SEX            EMPSTAT     
##  Min.   :1.000   Min.   :1.000   Male  : 95222   Min.   :0.000  
##  1st Qu.:2.000   1st Qu.:1.000   Female:101363   1st Qu.:1.000  
##  Median :2.000   Median :2.000                   Median :1.000  
##  Mean   :1.951   Mean   :1.691                   Mean   :1.514  
##  3rd Qu.:2.000   3rd Qu.:2.000                   3rd Qu.:3.000  
##  Max.   :2.000   Max.   :2.000                   Max.   :3.000  
##                                                                 
##     EMPSTATD        LABFORCE          OCC              IND       
##  Min.   : 0.00   Min.   :0.000   0      : 79987   0      :79987  
##  1st Qu.:10.00   1st Qu.:1.000   2310   :  3494   7860   : 9025  
##  Median :10.00   Median :2.000   5700   :  3235   8680   : 6354  
##  Mean   :15.16   Mean   :1.331   430    :  3025   770    : 6279  
##  3rd Qu.:30.00   3rd Qu.:2.000   4720   :  2666   8190   : 5873  
##  Max.   :30.00   Max.   :2.000   4760   :  2563   7870   : 4041  
##                                  (Other):101615   (Other):85026  
##     CLASSWKR       CLASSWKRD        WKSWORK2        UHRSWORK    
##  Min.   :0.000   Min.   : 0.00   Min.   :0.000   Min.   : 0.00  
##  1st Qu.:0.000   1st Qu.: 0.00   1st Qu.:0.000   1st Qu.: 0.00  
##  Median :2.000   Median :22.00   Median :1.000   Median :12.00  
##  Mean   :1.116   Mean   :13.03   Mean   :2.701   Mean   :19.77  
##  3rd Qu.:2.000   3rd Qu.:22.00   3rd Qu.:6.000   3rd Qu.:40.00  
##  Max.   :2.000   Max.   :29.00   Max.   :6.000   Max.   :99.00  
##                                                                 
##      INCTOT           FTOTINC           INCWAGE          POVERTY     
##  Min.   :  -7300   Min.   : -11800   Min.   :     0   Min.   :  0.0  
##  1st Qu.:   8000   1st Qu.:  35550   1st Qu.:     0   1st Qu.:159.0  
##  Median :  25000   Median :  74000   Median : 10000   Median :351.0  
##  Mean   :  45245   Mean   : 107110   Mean   : 33796   Mean   :318.7  
##  3rd Qu.:  56500   3rd Qu.: 132438   3rd Qu.: 47000   3rd Qu.:501.0  
##  Max.   :1563000   Max.   :2030000   Max.   :638000   Max.   :501.0  
##  NA's   :31129     NA's   :10817     NA's   :33427                   
##     MIGRATE1       MIGRATE1D        MIGPLAC1         MIGCOUNTY1     
##  Min.   :0.000   Min.   : 0.00   Min.   :  0.000   Min.   :  0.000  
##  1st Qu.:1.000   1st Qu.:10.00   1st Qu.:  0.000   1st Qu.:  0.000  
##  Median :1.000   Median :10.00   Median :  0.000   Median :  0.000  
##  Mean   :1.122   Mean   :11.51   Mean   :  6.184   Mean   :  4.117  
##  3rd Qu.:1.000   3rd Qu.:10.00   3rd Qu.:  0.000   3rd Qu.:  0.000  
##  Max.   :4.000   Max.   :40.00   Max.   :900.000   Max.   :810.000  
##                                                                     
##     MIGPUMA1        VETSTAT          VETSTATD         PWPUMA00    
##  Min.   :    0   Min.   :0.0000   Min.   : 0.000   Min.   :    0  
##  1st Qu.:    0   1st Qu.:1.0000   1st Qu.:11.000   1st Qu.:    0  
##  Median :    0   Median :1.0000   Median :11.000   Median :    0  
##  Mean   :  277   Mean   :0.8621   Mean   : 9.412   Mean   : 1255  
##  3rd Qu.:    0   3rd Qu.:1.0000   3rd Qu.:11.000   3rd Qu.: 3100  
##  Max.   :70100   Max.   :2.0000   Max.   :20.000   Max.   :59300  
##                                                                   
##     TRANWORK         TRANTIME         DEPARTS           in_NYC      
##  Min.   : 0.000   Min.   :  0.00   Min.   :   0.0   Min.   :0.0000  
##  1st Qu.: 0.000   1st Qu.:  0.00   1st Qu.:   0.0   1st Qu.:0.0000  
##  Median : 0.000   Median :  0.00   Median :   0.0   Median :0.0000  
##  Mean   : 9.725   Mean   : 14.75   Mean   : 373.3   Mean   :0.3615  
##  3rd Qu.:10.000   3rd Qu.: 20.00   3rd Qu.: 732.0   3rd Qu.:1.0000  
##  Max.   :70.000   Max.   :138.00   Max.   :2345.0   Max.   :1.0000  
##                                                                     
##     in_Bronx       in_Manhattan       in_StatenI       in_Brooklyn   
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.000  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000  
##  Median :0.0000   Median :0.00000   Median :0.00000   Median :0.000  
##  Mean   :0.0538   Mean   :0.04981   Mean   :0.02084   Mean   :0.126  
##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000  
##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.000  
##                                                                      
##    in_Queens      in_Westchester      in_Nassau          Hispanic     
##  Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.00000   Median :0.00000   Median :0.0000  
##  Mean   :0.1111   Mean   :0.04413   Mean   :0.07032   Mean   :0.1387  
##  3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.0000  
##                                                                       
##     Hisp_Mex          Hisp_PR         Hisp_Cuban         Hisp_DomR      
##  Min.   :0.00000   Min.   :0.0000   Min.   :0.000000   Min.   :0.00000  
##  1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.000000   1st Qu.:0.00000  
##  Median :0.00000   Median :0.0000   Median :0.000000   Median :0.00000  
##  Mean   :0.01626   Mean   :0.0436   Mean   :0.003403   Mean   :0.02827  
##  3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:0.000000   3rd Qu.:0.00000  
##  Max.   :1.00000   Max.   :1.0000   Max.   :1.000000   Max.   :1.00000  
##                                                                         
##      white             AfAm          Amindian            Asian        
##  Min.   :0.0000   Min.   :0.000   Min.   :0.000000   Min.   :0.00000  
##  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.000000   1st Qu.:0.00000  
##  Median :1.0000   Median :0.000   Median :0.000000   Median :0.00000  
##  Mean   :0.6997   Mean   :0.125   Mean   :0.003779   Mean   :0.08656  
##  3rd Qu.:1.0000   3rd Qu.:0.000   3rd Qu.:0.000000   3rd Qu.:0.00000  
##  Max.   :1.0000   Max.   :1.000   Max.   :1.000000   Max.   :1.00000  
##                                                                       
##     race_oth        unmarried       veteran        has_AnyHealthIns
##  Min.   :0.0000   Min.   :0.00   Min.   :0.00000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.00000   1st Qu.:1.0000  
##  Median :0.0000   Median :0.00   Median :0.00000   Median :1.0000  
##  Mean   :0.1324   Mean   :0.45   Mean   :0.04443   Mean   :0.9513  
##  3rd Qu.:0.0000   3rd Qu.:1.00   3rd Qu.:0.00000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.00   Max.   :1.00000   Max.   :1.0000  
##                                                                    
##  has_PvtHealthIns  Commute_car      Commute_bus      Commute_subway   
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.00000  
##  Median :1.0000   Median :0.0000   Median :0.00000   Median :0.00000  
##  Mean   :0.6906   Mean   :0.2997   Mean   :0.02162   Mean   :0.07468  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.00000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000  
##                                                                       
##   Commute_rail     Commute_other     below_povertyline below_150poverty
##  Min.   :0.00000   Min.   :0.00000   Min.   :0.000     Min.   :0.0000  
##  1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000     1st Qu.:0.0000  
##  Median :0.00000   Median :0.00000   Median :0.000     Median :0.0000  
##  Mean   :0.01332   Mean   :0.05506   Mean   :0.122     Mean   :0.1965  
##  3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000     3rd Qu.:0.0000  
##  Max.   :1.00000   Max.   :1.00000   Max.   :1.000     Max.   :1.0000  
##                                                                        
##  below_200poverty   foodstamps    
##  Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000  
##  Mean   :0.2676   Mean   :0.1465  
##  3rd Qu.:1.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000  
## 
```

As we can see from the data, An average of 0.3615 of people have been educated in New York  City, however, educational attainments in different neighborhoods in the New York city is differently, in Bronx, the mean is 0.0538, It is much lower than the average for New York as a whole, a testament to the low level of education in this area, in contrast, In Manhattan, the mean is 0.04981, In StatenI, the mean is 0.02084, In Brooklyn, the mean os 0.126, In Queens, the mean is 0.1111, the Brooklyn has lowest probability however Bronx has highest probability.

Question 5:
  I collect a month(from 08.06.2020-09.10.2020) data from this link https://finance.yahoo.com/quote/%5EGSPC/history/ to analyze the daily return of SP500 index, then i calculate it on excel and the mean of the daily return is 0.000992144.as you can see the following data.


|-----------------|----------|----------|----------|
|| Date     | Open     | High     | Low      | Close          | daily return |
| Sept\. 10, 2020 | 3412\.56 | 3425\.55 | 3329\.25 | 3339\.19 | 0\.023678      |
| Sept\. 9, 2020  | 3369\.82 | 3424\.77 | 3366\.84 | 3398\.96 | 0\.017584791   |
| Sept\. 8, 2020  | 3371\.88 | 3379\.97 | 3329\.27 | 3331\.84 | \-0\.020145025 |
| Sept\. 4, 2020  | 3453\.6  | 3479\.15 | 3349\.63 | 3426\.96 | 0\.027756379   |
| Sept\. 3, 2020  | 3564\.74 | 3564\.85 | 3427\.41 | 3455\.06 | 0\.008132999   |
| Sept\. 2, 2020  | 3543\.76 | 3588\.11 | 3535\.23 | 3580\.84 | 0\.035125836   |
| Sept\. 1, 2020  | 3507\.44 | 3528\.03 | 3494\.6  | 3526\.65 | \-0\.015365857 |
| Aug\. 31, 2020  | 3509\.73 | 3514\.77 | 3493\.25 | 3500\.31 | \-0\.007525048 |
| Aug\. 28, 2020  | 3494\.69 | 3509\.23 | 3484\.32 | 3508\.01 | 0\.002194977   |
| Aug\. 27, 2020  | 3485\.14 | 3501\.38 | 3468\.35 | 3484\.55 | \-0\.006732577 |
| Aug\. 26, 2020  | 3449\.97 | 3481\.07 | 3444\.15 | 3478\.73 | \-0\.001673024 |
| Aug\. 25, 2020  | 3435\.95 | 3444\.21 | 3425\.84 | 3443\.62 | \-0\.010195666 |
| Aug\. 24, 2020  | 3418\.09 | 3432\.09 | 3413\.13 | 3431\.28 | \-0\.003596326 |
| Aug\. 21, 2020  | 3386\.01 | 3399\.96 | 3379\.31 | 3397\.16 | \-0\.010043684 |
| Aug\. 20, 2020  | 3360\.48 | 3390\.8  | 3354\.69 | 3385\.51 | \-0\.003441136 |
| Aug\. 19, 2020  | 3392\.51 | 3399\.54 | 3369\.66 | 3374\.85 | \-0\.003158659 |
| Aug\. 18, 2020  | 3387\.04 | 3395\.06 | 3370\.15 | 3389\.78 | 0\.004404416   |
| Aug\. 17, 2020  | 3380\.86 | 3387\.59 | 3379\.22 | 3381\.99 | \-0\.002303378 |
| Aug\. 14, 2020  | 3368\.66 | 3378\.51 | 3361\.64 | 3372\.85 | \-0\.002709874 |
| Aug\. 13, 2020  | 3372\.95 | 3387\.24 | 3363\.35 | 3373\.43 | 0\.000171932   |
| Aug\. 12, 2020  | 3355\.46 | 3387\.89 | 3355\.46 | 3380\.35 | 0\.002047125   |
| Aug\. 11, 2020  | 3370\.34 | 3381\.01 | 3326\.44 | 3333\.69 | \-0\.013996502 |
| Aug\. 10, 2020  | 3356\.04 | 3363\.29 | 3335\.44 | 3360\.47 | 0\.007969123   |
| Aug\. 7, 2020   | 3340\.05 | 3352\.54 | 3328\.72 | 3351\.28 | \-0\.002742236 |
| Aug\. 6, 2020   | 3323\.17 | 3351\.03 | 3318\.14 | 3349\.16 | \-0\.000632995 |
|                 |          |          |          |          |                |

when the previous day’s return was positive,for example, on Aug 10 the day's return was positive, however on Aug 11,it was negative return. We also can see that from Aug 19 to Aug 27, they were all negative.The mean return on S&P500 index from August 26th to September 10th presented 11 days with positive return against 15 days with negative returns. There was no clear sequence on the positive or negative daily returns.  For instance, from September 2nd to September 4th the daily returns were positive, although on September 8 it was negative, then followed by positive returns for the remaining days.
In the decision-making process, investors can look for past returns of the S&P500 index and use it as a source of information for predicting future outcomes. Although, as studies has shown this kind of prediction can be biased, because streaks of last outcomes do not mean continued success. This assumption is called “Hot hands Fallacy”, a term brought from the belief that basketball players that score more in a game have greater probability of keeping their scores in the next games, which is not true, given the randomness of the event, according to JOSHUA B. MILLER and ADAM SANJURJO in the paper SURPRISED BY THE HOT HAND FALLACY? A TRUTH IN THE LAW OF SMALL NUMBERS: “…the bias can be leveraged to manipulate people into believing that the outcomes of an unpredictable process can be predicted at rates better than chance”.
“Hot hand fallacy” was a considered a cognitive social bias that a person who experiences a successful outcome has a greater chance of success in further attempts.
For example, investors piled into the fund during its run-up, with most inflows occurring near the investment’s peak. Investors then fled as the fund’s returns plummeted, with most outflows occurring near the investment’s bottom.Buying high and selling low is not a worthwhile investment philosophy! 
















## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
summary(cars)
```

```
##      speed           dist       
##  Min.   : 4.0   Min.   :  2.00  
##  1st Qu.:12.0   1st Qu.: 26.00  
##  Median :15.0   Median : 36.00  
##  Mean   :15.4   Mean   : 42.98  
##  3rd Qu.:19.0   3rd Qu.: 56.00  
##  Max.   :25.0   Max.   :120.00
```

## Including Plots

You can also embed plots, for example:

<img src="-first-homework_files/figure-html/pressure-1.png" width="672" />

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
