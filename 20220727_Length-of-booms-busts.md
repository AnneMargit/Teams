Length of booms/busts
================

Comparing short and long booms/busts Booms/busts can be between 3 and 9
measurements long. I recoded length of 6 and higher as long (so 6,7,8 or
9) with 2, and below 6 as short (so 3, 4, or 5) with 1.

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
    ##        AIC      BIC    logLik
    ##   11025.68 11055.63 -5507.838
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept) Residual
    ## StdDev:   0.6917398 1.524611
    ## 
    ## Fixed effects:  s.em1 ~ after.boom.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)          2.7115912 0.10711163 2905 25.315562  0.0000
    ## after.boom.length.d1 0.2145712 0.06957167 2905  3.084176  0.0021
    ## after.boom.length.d2 0.5330190 0.08421872 2905  6.328984  0.0000
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.boom.length.d1 -0.261       
    ## after.boom.length.d2 -0.144  0.145
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.1601796 -0.7242714 -0.1773803  0.6516410  3.2111597 
    ## 
    ## Number of Observations: 2957
    ## Number of Groups: 50

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
    ##   10852.01 10887.95 -5420.003
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.484831
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9211775 1.390897
    ## 
    ## Fixed effects:  s.b2 ~ after.boom.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)          2.7804710 0.09760554 2690 28.486815       0
    ## after.boom.length.d1 0.3013329 0.06347283 2690  4.747431       0
    ## after.boom.length.d2 0.5870051 0.07683827 2690  7.639489       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.boom.length.d1 -0.261       
    ## after.boom.length.d2 -0.144  0.145
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.20109708 -0.63528793 -0.06359526  0.58476679  3.10721018 
    ## 
    ## Number of Observations: 2957
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            50           265

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
    ##        AIC     BIC    logLik
    ##   25713.23 25753.9 -12850.61
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.4570556
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.6215473 1.671482
    ## 
    ## Fixed effects:  s.em1 ~ after.bust.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)           4.177249 0.06345863 5982  65.82634       0
    ## after.bust.length.d1 -1.372527 0.05034397 5982 -27.26299       0
    ## after.bust.length.d2 -1.388100 0.06453320 5982 -21.50986       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.bust.length.d1 -0.311       
    ## after.bust.length.d2 -0.189  0.149
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.4792823 -0.7066123 -0.1031516  0.6778554  3.4309828 
    ## 
    ## Number of Observations: 6495
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            94           511

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
    ##   26043.94 26084.61 -13015.97
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.4655157
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    0.636292 1.715422
    ## 
    ## Fixed effects:  s.b2 ~ after.bust.length.d 
    ##                          Value  Std.Error   DF   t-value p-value
    ## (Intercept)           4.206579 0.06481919 5981  64.89712       0
    ## after.bust.length.d1 -1.412205 0.05165670 5981 -27.33827       0
    ## after.bust.length.d2 -1.309962 0.06622580 5981 -19.78023       0
    ##  Correlation: 
    ##                      (Intr) af...1
    ## after.bust.length.d1 -0.313       
    ## after.bust.length.d2 -0.190  0.149
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.5820039 -0.7246061 -0.1120516  0.6774496  2.9883877 
    ## 
    ## Number of Observations: 6494
    ## Number of Groups: 
    ##          team pid %in% team 
    ##            94           511

> Burnout is slightly lower after a short (!) bust than after a long
> bust.
