During booms/busts: stress, burnout, feeling connected, interactions,
and social behavior
================

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

``` r
load(file="d.an.Rdata")
```

## Stress during boom/busts compared to other times

s.jd1 This afternoon, very few stressful things happened to me at work.

``` r
## Boom
model_1 <- lme(fixed = s.jd1 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit)

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   117036.7 117078.8 -58513.37
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5184048
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8011518 1.362641
    ## 
    ## Fixed effects:  s.jd1 ~ boom 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 5.074851 0.05082867 32326 99.84230       0
    ## boom        0.409631 0.04355440 32326  9.40504       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.03 
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.02661352 -0.42145367  0.09291163  0.61839588  3.57588514 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_1b <- lme(fixed = s.jd1 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit) 

summary(model_1b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   114756.6 114798.7 -57373.31
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5262545
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    0.802967 1.315348
    ## 
    ## Fixed effects:  s.jd1 ~ bust 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  5.193717 0.05136663 32326 101.11071       0
    ## bust        -1.438790 0.02901649 32326 -49.58525       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.041
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.28505526 -0.48765717  0.05546612  0.61125533  4.66999630 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> During booms, participants agreed more with ‘very few stressful things
> happened’. During busts, less agreement

s.b2 How burned out do you feel right now?

``` r
## Boom
model_2 <- lme(fixed = s.b2 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##      AIC      BIC    logLik
    ##   179974 180018.1 -89982.01
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5107952
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8175874 1.438686
    ## 
    ## Fixed effects:  s.b2 ~ boom 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.9227085 0.05049950 48896  57.87599       0
    ## boom        -0.4012548 0.03772964 48896 -10.63500       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.025
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.4195942 -0.6072025 -0.1225056  0.4084064  3.8621092 
    ## 
    ## Number of Observations: 49726
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_2b <- lme(fixed = s.b2 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit) 

summary(model_2b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   176975.8 177019.8 -88482.88
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5256983
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8187999 1.395152
    ## 
    ## Fixed effects:  s.b2 ~ bust 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.805596 0.05149559 48896 54.48226       0
    ## bust        1.430819 0.02523867 48896 56.69157       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.035
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.17583685 -0.59908738 -0.08526393  0.45913143  4.14230274 
    ## 
    ## Number of Observations: 49726
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants felt less burned out during booms and more during busts

s.jd2 This afternoon, I felt bothered or upset because of my job.

``` r
## Boom
model_3 <- lme(fixed = s.jd2 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC   logLik
    ##   118640.8 118682.8 -59315.4
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5218774
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7677759 1.398018
    ## 
    ## Fixed effects:  s.jd2 ~ boom 
    ##                  Value  Std.Error    DF  t-value p-value
    ## (Intercept)  2.9571121 0.05045761 32326 58.60587       0
    ## boom        -0.4219559 0.04468102 32326 -9.44374       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.031
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.47622813 -0.62259054 -0.09953367  0.45329170  4.02469622 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_3b <- lme(fixed = s.jd2 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_3b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##      AIC      BIC    logLik
    ##   116639 116681.1 -58314.52
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5273414
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7695772 1.355338
    ## 
    ## Fixed effects:  s.jd2 ~ bust 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.841389 0.05084511 32326 55.88322       0
    ## bust        1.389801 0.02989620 32326 46.48755       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.043
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.46188720 -0.62805864 -0.06478252  0.50482269  4.04878269 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants felt less bothered by things during booms and more during
> busts

s.jd3 This afternoon, I felt a great deal of stress because of my job.

``` r
## Boom
model_4 <- lme(fixed = s.jd3 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_4)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   118246.4 118288.5 -59118.22
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5133369
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7531156 1.390158
    ## 
    ## Fixed effects:  s.jd3 ~ boom 
    ##                  Value  Std.Error    DF  t-value p-value
    ## (Intercept)  2.9333057 0.04960620 32326 59.13184       0
    ## boom        -0.3794667 0.04442829 32326 -8.54111       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.031
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.4620996 -0.6367401 -0.1027585  0.4655771  4.0523802 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_4b <- lme(fixed = s.jd3 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_4b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   116095.3 116137.3 -58042.65
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5216211
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7550193 1.344584
    ## 
    ## Fixed effects:  s.jd3 ~ bust 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.816620 0.05018233 32326 56.12772       0
    ## bust        1.423495 0.02965865 32326 47.99594       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.043
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.55363469 -0.63612009 -0.06326253  0.51600240  4.18091523 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants felt less stressed during booms and more stressed during
> busts

s.jd4 This afternoon, I was under constant time pressure due to a heavy
workload.

``` r
## Boom
model_5 <- lme(fixed = s.jd4 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_5)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   117371.3 117413.3 -58680.65
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5128133
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7506901 1.371595
    ## 
    ## Fixed effects:  s.jd4 ~ boom 
    ##                  Value  Std.Error    DF  t-value p-value
    ## (Intercept)  2.9151646 0.04950962 32326 58.88077       0
    ## boom        -0.3824672 0.04383653 32326 -8.72485       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.031
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.4990600 -0.6199386 -0.1101044  0.4667747  4.0060500 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_5b <- lme(fixed = s.jd4 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_5b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   115084.2 115126.3 -57537.12
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5198825
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7527077 1.323852
    ## 
    ## Fixed effects:  s.jd4 ~ bust 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.796694 0.05000448 32326 55.92887       0
    ## bust        1.446285 0.02920209 32326 49.52677       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.043
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.63080775 -0.62206088 -0.06040856  0.50382810  4.25314961 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants felt less time pressured during booms and more during
> busts

## Interactions with team-mates during booms/busts

s.ip Please circle all of the teammates who you interacted with this
afternoon. Total = s.ip_1 + s.ip_2 + s.ip_3 + s.ip_4 + s.ip_5 + s.ip_6 +
s.ip_7

``` r
d.an <- d.an %>%
  dplyr::mutate(s.ip.t = rowSums(across(c(s.ip_1, s.ip_2, s.ip_3, s.ip_4, s.ip_5, s.ip_6, s.ip_7)), na.rm=T))
                
## Boom
model_6 <- lme(fixed = s.ip.t ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_6)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   213990.7 214034.8 -106990.4
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.6024537
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept) Residual
    ## StdDev: 1.512521e-16 2.069367
    ## 
    ## Fixed effects:  s.ip.t ~ boom 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.6746571 0.04933255 48900 54.21688  0.0000
    ## boom        0.0360358 0.05420703 48900  0.66478  0.5062
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.037
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -1.8572635 -1.1802958  0.5293761  0.7557622  1.1768265 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_6b <- lme(fixed = s.ip.t ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_6b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   213990.7 214034.8 -106990.3
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.602349
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev: 1.51292e-16 2.069352
    ## 
    ## Fixed effects:  s.ip.t ~ bust 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept) 2.6729115 0.04936372 48900 54.14729  0.0000
    ## bust        0.0411677 0.03739136 48900  1.10100  0.2709
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.055
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -1.8752078 -1.1802739  0.5294137  0.7570743  1.1807137 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants did not interact with more or fewer team mates during
> booms/busts

s.il Approximately how many minutes did you spend in interactions with
each team member this afternoon?

``` r
# Total number of minutes
d.an <- d.an %>%
  dplyr::mutate(s.il.t = rowSums(across(c(s.il_1, s.il_2, s.il_3, s.il_4, s.il_5, s.il_6, s.il_7)), na.rm=T))

# There are some participants who said they didn't interact with anyone (s.ip.t = 0) but nonetheless said they interacted X number of minutes in total. I set the number of minutes to 0 for these participants (78 measurements). 

d.an <- d.an %>%
  mutate(s.il.t = ifelse(s.ip.t == 0, 0, s.il.t))

# Mean number of minutes per team mate
d.an <- d.an %>%
  dplyr::mutate(s.il.m = s.il.t / s.ip.t)

d.an$s.il.m[is.na(d.an$s.il.m)] <- 0 

## Boom
model_7 <- lme(fixed = s.il.m ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_7)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   432976.6 433020.7 -216483.3
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    2.775078
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept) Residual
    ## StdDev: 5.659386e-12 18.74675
    ## 
    ## Fixed effects:  s.il.m ~ boom 
    ##                 Value Std.Error    DF  t-value p-value
    ## (Intercept) 21.252651 0.2392029 48900 88.84780  0.0000
    ## boom         0.971186 0.4884535 48900  1.98829  0.0468
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.069
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -1.68657115 -1.03450761  0.08598719  0.69365794 12.45167317 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_7b <- lme(fixed = s.il.m ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_7b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   432910.4 432954.5 -216450.2
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    2.759111
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev: 5.81945e-12 18.73439
    ## 
    ## Fixed effects:  s.il.m ~ bust 
    ##                 Value Std.Error    DF  t-value p-value
    ## (Intercept) 21.080495 0.2386530 48900 88.33117       0
    ## bust         2.838926 0.3370122 48900  8.42381       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.102
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -1.83067808 -1.02755353  0.09063537  0.69588286 12.46797416 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants had longer interactions during booms and even longer
> interactions during busts

s.ie How much did you enjoy interacting with each team member this
afternoon?

``` r
# Total enjoyment
d.an <- d.an %>%
  dplyr::mutate(s.ie.t = rowSums(across(c(s.ie_1, s.ie_2, s.ie_3, s.ie_4, s.ie_5, s.ie_6, s.ie_7)), na.rm=T))

# There are some participants who said they didn't interact with anyone (s.ip.t = 0) but nonetheless said they enjoyed the interaction. I set the number of minutes to 0 for these participants (78 measurements). 

d.an <- d.an %>%
  mutate(s.ie.t = ifelse(s.ip.t == 0, 0, s.ie.t))

# Mean number of minutes per team mate
d.an <- d.an %>%
  dplyr::mutate(s.ie.m = s.ie.t / s.ip.t)

d.an$s.ie.m[is.na(d.an$s.ie.m)] <- 0 

## Boom
model_8 <- lme(fixed = s.ie.m ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_8)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   236598.9 236642.9 -118294.4
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3601672
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.2428527 2.595299
    ## 
    ## Fixed effects:  s.ie.m ~ boom 
    ##                Value  Std.Error    DF   t-value p-value
    ## (Intercept) 3.263835 0.03246342 48900 100.53884       0
    ## boom        0.456173 0.06760004 48900   6.74812       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.071
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -2.0026345 -1.1671315  0.4197762  0.7687268 13.3281810 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_8b <- lme(fixed = s.ie.m ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_8b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   236256.3 236300.4 -118123.2
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3675834
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.2445658 2.586069
    ## 
    ## Fixed effects:  s.ie.m ~ bust 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  3.345862 0.03308795 48900 101.12025       0
    ## bust        -0.919611 0.04652372 48900 -19.76649       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.102
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -1.9286273 -1.1635259  0.4135017  0.7569228 13.3206495 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants enjoyed interactions more during booms and less during
> busts

## Social behavior: how rude, kind, gregarious, and talkative were participants during booms/busts?

s.ps1 How rude were you to your teammates this morning/afternoon?

``` r
## Boom
model_9 <- lme(fixed = s.ps1 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_9)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   116055.8 116097.8 -58022.88
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3553962
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8977321 1.340202
    ## 
    ## Fixed effects:  s.ps1 ~ boom 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  3.180960 0.04309580 32326  73.81137       0
    ## boom        -0.767923 0.04281649 32326 -17.93522       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.034
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.68514539 -0.61207857 -0.07268485  0.43065745  4.13305420 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_9b <- lme(fixed = s.ps1 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_9b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   114410.6 114452.7 -57200.32
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3646086
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8985504 1.306458
    ## 
    ## Fixed effects:  s.ps1 ~ bust 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 3.059822 0.04360923 32326 70.16455       0
    ## bust        1.297322 0.02881002 32326 45.03024       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.048
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.61352536 -0.57195956 -0.05688454  0.47030139  4.28366814 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants were less rude to team mates during booms and more during
> busts

s.ps2 How gregarious were you around your teammates this morning?

``` r
## Boom
model_10 <- lme(fixed = s.ps2 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_10)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   119039.8 119081.9 -59514.91
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3725326
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8351013 1.405505
    ## 
    ## Fixed effects:  s.ps2 ~ boom 
    ##                Value  Std.Error    DF   t-value p-value
    ## (Intercept) 4.833091 0.04257102 32326 113.53006       0
    ## boom        0.784544 0.04489229 32326  17.47613       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.036
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.94189470 -0.47157237  0.09450302  0.61861298  3.69548970 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_10b <- lme(fixed = s.ps2 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_10b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##      AIC      BIC    logLik
    ##   117580 117622.1 -58785.02
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3825991
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8359368 1.374039
    ## 
    ## Fixed effects:  s.ps2 ~ bust 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.954225 0.04316525 32326 114.77347       0
    ## bust        -1.290439 0.03029506 32326 -42.59568       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.051
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.05906228 -0.52217421  0.07337681  0.60123719  4.28557506 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants were more gregarious to team mates during booms and less
> during busts

s.ps3 How kind, considerate were you to your teammates this morning?

``` r
## Boom
model_11 <- lme(fixed = s.ps3 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_11)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   119783.7 119825.8 -59886.86
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3809026
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8132315 1.422483
    ## 
    ## Fixed effects:  s.ps3 ~ boom 
    ##                Value  Std.Error    DF   t-value p-value
    ## (Intercept) 4.852441 0.04255550 32326 114.02618       0
    ## boom        0.713947 0.04543224 32326  15.71455       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.037
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.7231005 -0.5012486  0.1110844  0.6463424  3.5013248 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_11b <- lme(fixed = s.ps3 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_11b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   118323.6 118365.7 -59156.82
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3867615
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8145031 1.390653
    ## 
    ## Fixed effects:  s.ps3 ~ bust 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.970638 0.04292479 32326 115.79877       0
    ## bust        -1.283795 0.03065969 32326 -41.87241       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.052
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.90374352 -0.54427292  0.09642153  0.61995455  4.32933828 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants were more kind and considerate to team mates during booms
> and less during busts

s.ps4 How talkative were you around your teammates this morning?

``` r
## Boom
model_12 <- lme(fixed = s.ps4 ~ boom,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_12)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   118382.5 118424.6 -59186.26
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3803009
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8013454  1.39248
    ## 
    ## Fixed effects:  s.ps4 ~ boom 
    ##                Value  Std.Error    DF   t-value p-value
    ## (Intercept) 4.872359 0.04221353 32326 115.42175       0
    ## boom        0.710579 0.04447647 32326  15.97652       0
    ##  Correlation: 
    ##      (Intr)
    ## boom -0.036
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.9269833 -0.4711543  0.1091754  0.6335512  3.3774180 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Bust
model_12b <- lme(fixed = s.ps4 ~ bust,
                   random = ~1 | team/pid, 
                   data = d.an, 
                   na.action = na.omit, control = lmeControl(opt = "optim")) 

summary(model_12b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.an 
    ##        AIC      BIC    logLik
    ##   116693.2 116735.3 -58341.62
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.390598
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.8027342 1.356465
    ## 
    ## Fixed effects:  s.ps4 ~ bust 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.994467 0.04284447 32326 116.57203       0
    ## bust        -1.339141 0.02990806 32326 -44.77526       0
    ##  Correlation: 
    ##      (Intr)
    ## bust -0.051
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.03781232 -0.51035200  0.08611311  0.61333634  3.97548063 
    ## 
    ## Number of Observations: 33156
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

> Participants were more talkative during booms and less during busts

s.c How connected do you feel with the people on your team right now?
s.b2 How burned out do you feel right now? s.jd1 This afternoon, very
few stressful things happened to me at work. s.jd2 This afternoon, I
felt bothered or upset because of my job. s.jd3 This afternoon, I felt a
great deal of stress because of my job. s.jd4 This afternoon, I was
under constant time pressure due to a heavy workload.

s.ps1 How rude were you to your teammates this morning? s.ps2 How
gregarious were you around your teammates this morning? s.ps3 How kind,
considerate were you to your teammates this morning? s.ps4 How talkative
were you around your teammates this morning?

s.ip Please circle all of the teammates who you interacted with this
afternoon. Total = s.ip_1 + s.ip_2 + s.ip_3 + s.ip_4 + s.ip_5 + s.ip_6 +
s.ip_7 s.im How did you primarily interact with each team member this
afternoon? s.il Approximately how many minutes did you spend in
interactions with each team member this afternoon? s.ie How much did you
enjoy interacting with each team member this afternoon?
