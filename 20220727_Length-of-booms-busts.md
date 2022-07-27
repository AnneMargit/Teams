Length of booms/busts
================

Comparing short and long booms/busts Booms/busts can be between 3 and 9
measurements long. I recoded length of 6 and higher as long (so 6,7,8 or
9) with 2, and below 6 as short (so 3, 4, or 5) with 1, periods with no
booms/busts as 0 (variable = length.boom.d or length.bust.d)

I created dummy variables to indicate whether the measurement occured
after a long or short boom/bust. This is the column after.boom.length.d
or after.bust.length.d (2 = after long one, 1 is after short one, 0 =
during boom/bust)

``` r
library(knitr)
library(dplyr)
library(anomaly)
library(tidyverse)
library(skimr)
library(nlme)
library(ggplot2)
library(effects)
library(psych)
```

Load anomaly data

``` r
load(file="d.an.Rdata")

# Remove teams with no anomalies
d.an <- d.an %>%
  filter(!is.na(anomalies.count))
```

Stress during/after a boom The column after.boom.length.d indicates the
difference between a long and short boom (2 = after long boom, 1 is
after short boom, 0 = during boom/bust) –\> so this compares stress
after a long/short boom vs. during

``` r
d.an$after.boom.length.d <- as.factor(d.an$after.boom.length.d)

model_1 <- lme(fixed = s.em1 ~ after.boom.length.d,
                   random = ~1 | team, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##       AIC      BIC    logLik
    ##   10359.4 10389.03 -5174.698
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept) Residual
    ## StdDev:    0.683223 1.531134
    ## 
    ## Fixed effects:  s.em1 ~ after.boom.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)          2.7100157 0.10769667 2720 25.163412   0e+00
    ## after.boom.length.d1 0.2444800 0.07200162 2720  3.395480   7e-04
    ## after.boom.length.d2 0.5440619 0.08672111 2720  6.273696   0e+00
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.boom.length.d1 -0.283       
    ## after.boom.length.d2 -0.152  0.158
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.1451722 -0.7299547 -0.1858405  0.6422382  3.1992071 
    ## 
    ## Number of Observations: 2771
    ## Number of Groups: 49

> Stress is higher after a long boom than after a short boom

Burnout

``` r
model_2 <- lme(fixed = s.b2 ~ after.boom.length.d,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   10233.59 10269.15 -5110.796
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.4951967
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9204454 1.403811
    ## 
    ## Fixed effects:  s.b2 ~ after.boom.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)          2.7770945 0.10024250 2509 27.703763       0
    ## after.boom.length.d1 0.3019797 0.06604739 2509  4.572167       0
    ## after.boom.length.d2 0.5855631 0.07953066 2509  7.362734       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.boom.length.d1 -0.278       
    ## after.boom.length.d2 -0.149  0.157
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.16472889 -0.63758920 -0.04643264  0.59928997  3.07855670 
    ## 
    ## Number of Observations: 2771
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            49           260

> Burnout is higher after a long boom than after a short boom

Stress during/after bust

``` r
d.an$after.bust.length.d <- as.factor(d.an$after.bust.length.d)

model_3 <- lme(fixed = s.em1 ~ after.bust.length.d,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   24326.55 24366.88 -12157.27
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.4408671
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.6291071 1.670857
    ## 
    ## Fixed effects:  s.em1 ~ after.bust.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)           4.179656 0.06324386 5646  66.08794       0
    ## after.bust.length.d1 -1.357323 0.05261501 5646 -25.79725       0
    ## after.bust.length.d2 -1.416805 0.06537477 5646 -21.67205       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.bust.length.d1 -0.316       
    ## after.bust.length.d2 -0.191  0.136
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.4784948 -0.7115962 -0.1047859  0.6795173  3.4228276 
    ## 
    ## Number of Observations: 6143
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            91           495

> Stress is slightly lower after a long bust than after a short bust.

Burnout during/after bust

``` r
model_4 <- lme(fixed = s.b2 ~ after.bust.length.d,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_4)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   24700.14 24740.47 -12344.07
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.4670573
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.6301947 1.724829
    ## 
    ## Fixed effects:  s.b2 ~ after.bust.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)           4.201785 0.06583599 5645  63.82201       0
    ## after.bust.length.d1 -1.385759 0.05433866 5645 -25.50227       0
    ## after.bust.length.d2 -1.334222 0.06750759 5645 -19.76404       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.bust.length.d1 -0.313       
    ## after.bust.length.d2 -0.189  0.136
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.5580060 -0.7269500 -0.1166661  0.6819554  2.9593279 
    ## 
    ## Number of Observations: 6142
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            91           495

> Burnout is slightly lower after a short (!) bust than after a long
> bust.
