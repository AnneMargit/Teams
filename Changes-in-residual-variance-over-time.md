Changes in residual variance over time
================

> The emergence of consensus (or the opposite, increases in divergence)
> can be captured by modeling changes in residual variances over time in
> multilevel growth models.

> A two-level model estimates this through including (1) a time variable
> that accounts for change in the group means over time and (2) a
> variance function to account for changes in the residual variance over
> time.

> See also Lang et al. 2019 (<http://dx.doi.org/10.1037/0000115-023>)

``` r
library(knitr)
library(dplyr)
library(tidyverse)
library (nlme)
```

Step 0: Add time variable (= measurement count)

``` r
load("d.Rdata")

d <- d %>%
  select(team, pid, day, type, s.pas)

d$team <- as.factor(d$team)
d$pid <- as.factor(d$pid)

d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(m = 1:n()) %>%
  ungroup()
```

Step 1: Examine change in groups’ latent means over time, i.e. does
group-level passion increase or decrease over time?

``` r
step1 <- lme(s.pas ~ m, random =
             list(team = pdDiag(~1), pid = pdSymm(~1)),
           data = d, na.action = na.omit)

summary(step1)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC      BIC    logLik
    ##   171827.7 171871.8 -85908.86
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5482988
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7523438 1.324805
    ## 
    ## Fixed effects: s.pas ~ m 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept)  5.080048 0.05275612 48900 96.29306       0
    ## m           -0.003220 0.00034304 48900 -9.38585       0
    ##  Correlation: 
    ##   (Intr)
    ## m -0.198
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.3812180 -0.3798278  0.1084483  0.5613508  3.7672266 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
VarCorr(step1)
```

    ##             Variance  StdDev   
    ## team =      pdDiag(1)          
    ## (Intercept) 0.3006316 0.5482988
    ## pid =       pdSymm(1)          
    ## (Intercept) 0.5660212 0.7523438
    ## Residual    1.7551077 1.3248048

> Average passion for work decreases slightly over time

Step 2: Do some teams increase or decrease more in passion over time
than the average, i.e. examining slope variability

``` r
step2a <-update(step1, random = list(team = pdDiag(~m), pid=pdSymm(~1)))
summary(step2a)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC      BIC    logLik
    ##   171394.3 171447.1 -85691.13
    ## 
    ## Random effects:
    ##  Formula: ~m | team
    ##  Structure: Diagonal
    ##         (Intercept)           m
    ## StdDev:   0.5909049 0.008767832
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7526072 1.315201
    ## 
    ## Fixed effects: s.pas ~ m 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept)  5.085699 0.05564946 48900 91.38811       0
    ## m           -0.003464 0.00078406 48900 -4.41805       0
    ##  Correlation: 
    ##   (Intr)
    ## m -0.083
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.3490207 -0.3933723  0.1148340  0.5595396  3.7145906 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
VarCorr(step2a)
```

    ##             Variance     StdDev     
    ## team =      pdDiag(m)               
    ## (Intercept) 3.491685e-01 0.590904855
    ## m           7.687487e-05 0.008767832
    ## pid =       pdSymm(1)               
    ## (Intercept) 5.664176e-01 0.752607212
    ## Residual    1.729753e+00 1.315200700

``` r
step2b <-update(step2a, random = list(team = pdSymm(~m), pid=pdSymm(~1)), 
                control = lmeControl(opt = "optim"))

summary(step2b)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC      BIC    logLik
    ##   171374.5 171436.2 -85680.24
    ## 
    ## Random effects:
    ##  Formula: ~m | team
    ##  Structure: General positive-definite
    ##             StdDev      Corr  
    ## (Intercept) 0.614778018 (Intr)
    ## m           0.009089853 -0.452
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7526307 1.315075
    ## 
    ## Fixed effects: s.pas ~ m 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept)  5.087764 0.05729913 48900 88.79304       0
    ## m           -0.003473 0.00080744 48900 -4.30072       0
    ##  Correlation: 
    ##   (Intr)
    ## m -0.431
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.3617151 -0.3930259  0.1141021  0.5599025  3.7160024 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
VarCorr(step2b)
```

    ##             Variance     StdDev      Corr  
    ## team =      pdSymm(m)                      
    ## (Intercept) 3.779520e-01 0.614778018 (Intr)
    ## m           8.262543e-05 0.009089853 -0.452
    ## pid =       pdSymm(1)                      
    ## (Intercept) 5.664529e-01 0.752630664       
    ## Residual    1.729423e+00 1.315075452

``` r
anova(step1, step2a, step2b)
```

    ##        Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## step1      1  5 171827.7 171871.8 -85908.86                        
    ## step2a     2  6 171394.3 171447.1 -85691.13 1 vs 2 435.4597  <.0001
    ## step2b     3  7 171374.5 171436.2 -85680.24 2 vs 3  21.7895  <.0001

> Including a random slope improves the model fit, which suggests that
> there is variability between groups in group-level passion change over
> time.

Step 3: Is there an increase or decrease in passion consensus within
groups? I.e. do group members’ passion levels become more or less
homogeneous over time?

``` r
step3 <- update(step2b, weights = varExp( form = ~ m))

anova(step1, step2a, step2b, step3)
```

    ##        Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## step1      1  5 171827.7 171871.8 -85908.86                        
    ## step2a     2  6 171394.3 171447.1 -85691.13 1 vs 2 435.4597  <.0001
    ## step2b     3  7 171374.5 171436.2 -85680.24 2 vs 3  21.7895  <.0001
    ## step3      4  8 171328.5 171399.0 -85656.24 3 vs 4  47.9848  <.0001

``` r
summary(step3)$tTable
```

    ##                    Value    Std.Error    DF   t-value      p-value
    ## (Intercept)  5.090894714 0.0574257180 48900 88.651825 0.000000e+00
    ## m           -0.003575069 0.0008157949 48900 -4.382313 1.176702e-05

``` r
VarCorr(step3)
```

    ##             Variance     StdDev      Corr  
    ## team =      pdSymm(m)                      
    ## (Intercept) 3.809975e-01 0.617249932 (Intr)
    ## m           8.474144e-05 0.009205512 -0.46 
    ## pid =       pdSymm(1)                      
    ## (Intercept) 5.669786e-01 0.752979811       
    ## Residual    1.593925e+00 1.262507562

``` r
summary(step3)$modelStruct$varStruct
```

    ## Variance function structure of class varExp representing
    ##       expon 
    ## 0.001319858

> The error term estimated in the last step reflects the percentage
> increase in the standard deviation of the errors with each unit
> increase in the time variable.

> So the residual variance at measurement 0 = 1.59 \* (1.59 \* exp\[2 \*
> 0.0013 \* 0\]) = 2.5281

> The residual variance at measurement 60 = 1.59 \* (1.59 \* exp\[2 \*
> 0.0013 \* 60\]) = 2.95491

> In other words, over the whole study period, team members become less
> similar to each other in how passionate they are (!)
