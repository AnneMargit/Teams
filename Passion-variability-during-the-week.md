Passion variability during the week
================

``` r
library(knitr)
library(dplyr)
library(tidyverse)
library(nlme)
library(lattice)
library(skimr)
library(ggplot2)
library(effects)
library(relativeVariability)
library(multilevel)
library(tidyr)
```

## Prep

Create measurement indicator (m) and calculate within-team average
(mean.pas) and standard deviation (sd.pas) of passion (on each
measurement occasion). Calculate also average per day (mean.pas.d) and
sd per day (sd.pas.d). Create week-indicator (week)

``` r
load(file="d.Rdata")

d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(m = 1:n()) %>%
  ungroup()

d$team <- as.factor(d$team)
d$pid <- as.factor(d$pid)

# There are 10 measurements with missings on passion, I delete these 
d <- d %>%
  filter(!is.na(s.pas))

d <- d %>%
  group_by(team, m) %>%
  mutate(sd.pas = sd(s.pas),
         mean.pas = mean(s.pas)) %>%
  ungroup()

# Passion SD and mean

d <- d %>%
  group_by(team, day) %>%
  mutate(sd.pas.d = sd(s.pas),
         mean.pas.d = mean(s.pas)) %>%
  ungroup()

# Week indicator

d <- d %>%
  mutate(week = case_when(
    day > 0 & day < 8 ~ 1,
    day > 7 & day < 15 ~2,
    day > 14 & day < 22 ~3))

# New day variable to indicate day of the week (1,2,3,4,5,6,7, 1,2,3,4,5,6,7 etc)

d <- d %>%
  mutate(day.new = case_when(
    day == 1 | day == 8 | day == 15 ~ 1,
    day == 2 | day == 9 | day == 16 ~ 2,
    day == 3 | day == 10 | day == 17 ~ 3,
    day == 4 | day == 11 | day == 18 ~ 4,
    day == 5 | day == 12 | day == 19 ~ 5,
    day == 6 | day == 13 | day == 20 ~ 6,
    day == 7 | day == 14 ~ 7))
```

## Does passion variability change throughout the week?

With day of the week (1-7) as predictor and daily SD (!) as dependent
variable:

``` r
# Random intercept (+ correcting for the mean)
model_1 <- lme(fixed = sd.pas.d ~ day.new + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   14718.54 14771.43 -7353.272
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3711727
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)  Residual
    ## StdDev: 9.053212e-06 0.2777008
    ## 
    ## Fixed effects:  sd.pas.d ~ day.new + mean.pas.d 
    ##                  Value   Std.Error    DF   t-value p-value
    ## (Intercept)  2.0066349 0.031346573 48899  64.01449       0
    ## day.new     -0.0068842 0.000646695 48899 -10.64522       0
    ## mean.pas.d  -0.1442087 0.001825598 48899 -78.99257       0
    ##  Correlation: 
    ##            (Intr) day.nw
    ## day.new    -0.100       
    ## mean.pas.d -0.296  0.072
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -5.5717239 -0.4699870 -0.1104961  0.3370246  7.1380503 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slope for day on team and participant level
model_1b <- lme(fixed = sd.pas.d ~ day.new + mean.pas.d,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d, 
                   na.action = na.omit)

summary(model_1b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   12998.01 13086.15 -6489.005
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.37802230 (Intr)
    ## day.new     0.02976523 -0.208
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 4.155558e-06 (Intr)
    ## day.new     1.958242e-06 -0.001
    ## Residual    2.717549e-01       
    ## 
    ## Fixed effects:  sd.pas.d ~ day.new + mean.pas.d 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.0002197 0.03190014 48899  62.70254  0.0000
    ## day.new     -0.0065594 0.00247562 48899  -2.64962  0.0081
    ## mean.pas.d  -0.1431714 0.00184288 48899 -77.68916  0.0000
    ##  Correlation: 
    ##            (Intr) day.nw
    ## day.new    -0.217       
    ## mean.pas.d -0.294  0.021
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.03556089 -0.47547414 -0.09031138  0.36762704  7.10041655 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Compare likelihoods (using ML instead of REML, otherwise likelihoods are not comparable)
model_1r <- lme(fixed = sd.pas.d ~ day.new + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_1br <- lme(fixed = sd.pas.d ~ day.new + mean.pas.d,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_1r, model_1br)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_1r      1  6 14689.73 14742.61 -7338.864                        
    ## model_1br     2 10 12971.89 13060.03 -6475.945 1 vs 2 1725.838  <.0001

``` r
# Best model is model with random slopes for day on team and participant level. There is a very  slight decrease in daily passion SD (-0.0065) over the course of the week 
```

With day of the week (1-7) as predictor and SD at each measurement (!)
as dependent variable:

``` r
model_1c <- lme(fixed = sd.pas ~ day.new + mean.pas,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_1c)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC     BIC    logLik
    ##   57406.82 57459.7 -28697.41
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3534779
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)  Residual
    ## StdDev: 1.229775e-05 0.4271983
    ## 
    ## Fixed effects:  sd.pas ~ day.new + mean.pas 
    ##                  Value   Std.Error    DF   t-value p-value
    ## (Intercept)  2.3610648 0.031210214 48899  75.65039       0
    ## day.new     -0.0081805 0.000994147 48899  -8.22864       0
    ## mean.pas    -0.2183896 0.002407630 48899 -90.70729       0
    ##  Correlation: 
    ##          (Intr) day.nw
    ## day.new  -0.146       
    ## mean.pas -0.392  0.062
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.50027493 -0.54640930 -0.05118984  0.49779522  5.12043711 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slope for day on team and participant level
model_1d <- lme(fixed = sd.pas ~ day.new + mean.pas,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d, 
                   na.action = na.omit)

summary(model_1d)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   56721.53 56809.67 -28350.77
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.36058397 (Intr)
    ## day.new     0.03138077 -0.229
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 8.083642e-06 (Intr)
    ## day.new     2.415763e-06 -0.001
    ## Residual    4.229046e-01       
    ## 
    ## Fixed effects:  sd.pas ~ day.new + mean.pas 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.3702178 0.03179323 48899  74.55103  0.0000
    ## day.new     -0.0080162 0.00271086 48899  -2.95707  0.0031
    ## mean.pas    -0.2203532 0.00243399 48899 -90.53153  0.0000
    ##  Correlation: 
    ##          (Intr) day.nw
    ## day.new  -0.248       
    ## mean.pas -0.390  0.026
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.64306615 -0.54562901 -0.05608596  0.49632324  5.06614515 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Compare likelihoods 
model_1cr <- lme(fixed = sd.pas ~ day.new + mean.pas,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_1dr <- lme(fixed = sd.pas ~ day.new + mean.pas,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_1cr, model_1dr)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_1cr     1  6 57379.32 57432.21 -28683.66                        
    ## model_1dr     2 10 56696.05 56784.19 -28338.02 1 vs 2 691.2727  <.0001

``` r
# The second model is better. It shows similar results as when looking at daily SD passion variability: over the course of the week, within-team passion variability decreases slightly (-0.008) 
```

## Using the relative standard deviation

Relative variability index \>\>\> this is a measure of variability that
is not confounded by the mean. See also Mestdagh et al. (2018,
<http://dx.doi.org/10.1037/met0000153>).

``` r
# Calculate relative variability rl.sd
rel.sd <- function(x) {
  relativeSD(x, MIN = 1, MAX=7)
}

d2 <- d %>%
  group_by(team, m) %>%
  mutate(rl.sd = rel.sd(s.pas))
```

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## minimum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

``` r
# Daily relative sd = rl.sd.d
d2 <- d2 %>%
  group_by(team, day) %>%
  mutate(rl.sd.d = rel.sd(s.pas))
```

    ## Warning in checkOutput(M, MIN, MAX): NaN returned. Data has a mean equal the
    ## maximum

Day.new as predictor, daily relative SD as dependent:

``` r
# Random intercept
model_2 <- lme(fixed = rl.sd.d ~ day.new,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##         AIC       BIC   logLik
    ##   -84805.98 -84761.91 42407.99
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.1355631
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)  Residual
    ## StdDev: 3.138167e-06 0.1020776
    ## 
    ## Fixed effects:  rl.sd.d ~ day.new 
    ##                  Value   Std.Error    DF   t-value p-value
    ## (Intercept)  0.4784526 0.010936794 48885  43.74706       0
    ## day.new     -0.0024911 0.000237126 48885 -10.50550       0
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.083
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.3377039 -0.5225060 -0.1295993  0.3722220  6.9145790 
    ## 
    ## Number of Observations: 49715
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slopes for day on team and pid level
model_2b <- lme(fixed = rl.sd.d ~ day.new,
                    random = list(team = ~day.new, pid = ~day.new), 
                   data = d2, 
                   na.action = na.omit)

summary(model_2b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##         AIC       BIC   logLik
    ##   -86672.15 -86592.82 43345.07
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.13531117 (Intr)
    ## day.new     0.01143603 -0.157
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 4.207076e-07 (Intr)
    ## day.new     6.657330e-07 -0.001
    ## Residual    9.973456e-02       
    ## 
    ## Fixed effects:  rl.sd.d ~ day.new 
    ##                  Value   Std.Error    DF  t-value p-value
    ## (Intercept)  0.4777503 0.010915523 48885 43.76797   0.000
    ## day.new     -0.0023074 0.000948157 48885 -2.43353   0.015
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.172
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.8898047 -0.4956018 -0.1249503  0.3697617  6.4840673 
    ## 
    ## Number of Observations: 49715
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Check likelihoods
model_2r <- lme(fixed = rl.sd.d ~ day.new,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

model_2br <- lme(fixed = rl.sd.d ~ day.new,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d2, 
                   na.action = na.omit, method = "ML")

anova(model_2r, model_2br)
```

    ##           Model df       AIC       BIC   logLik   Test  L.Ratio p-value
    ## model_2r      1  5 -84828.04 -84783.97 42419.02                        
    ## model_2br     2  9 -86691.47 -86612.14 43354.73 1 vs 2 1871.423  <.0001

``` r
# Second model is better. This again shows similar results: a decrease in relative SD over the course of the week (-0.0023)
```

Day.new as predictor, relative SD per measurement (!) as dependent:

``` r
# Random intercept
model_2c <- lme(fixed = rl.sd ~ day.new,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_2c)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##      AIC       BIC  logLik
    ##   -38679 -38634.94 19344.5
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.139516
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)  Residual
    ## StdDev: 5.325756e-06 0.1625158
    ## 
    ## Fixed effects:  rl.sd ~ day.new 
    ##                  Value   Std.Error    DF  t-value p-value
    ## (Intercept)  0.4890680 0.011324389 48855 43.18714       0
    ## day.new     -0.0034112 0.000377605 48855 -9.03386       0
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.128
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.84501534 -0.57117881 -0.08305093  0.49032296  4.68753694 
    ## 
    ## Number of Observations: 49685
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slopes for day on team and pid level
model_2d <- lme(fixed = rl.sd ~ day.new,
                    random = list(team = ~day.new, pid = ~day.new), 
                   data = d2, 
                   na.action = na.omit)

summary(model_2d)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##         AIC      BIC   logLik
    ##   -39434.52 -39355.2 19726.26
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.13692938 (Intr)
    ## day.new     0.01255811 -0.123
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 3.581687e-06 (Intr)
    ## day.new     5.874610e-07 0     
    ## Residual    1.607529e-01       
    ## 
    ## Fixed effects:  rl.sd ~ day.new 
    ##                  Value   Std.Error    DF  t-value p-value
    ## (Intercept)  0.4884406 0.011118769 48855 43.92938  0.0000
    ## day.new     -0.0032457 0.001077382 48855 -3.01262  0.0026
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.16 
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.86635276 -0.57413568 -0.07423232  0.48672090  4.76418137 
    ## 
    ## Number of Observations: 49685
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Check likelihoods
model_2cr <- lme(fixed = rl.sd ~ day.new,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

model_2dr <- lme(fixed = rl.sd ~ day.new,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d2, 
                   na.action = na.omit, method = "ML")

anova(model_2cr, model_2dr)
```

    ##           Model df       AIC       BIC   logLik   Test  L.Ratio p-value
    ## model_2cr     1  5 -38700.07 -38656.00 19355.04                        
    ## model_2dr     2  9 -39453.54 -39374.22 19735.77 1 vs 2 761.4699  <.0001

``` r
# Second model is better. Also a slight decrease in relative SD over the course of the week
```

## Within-group agreement (rwg)

The rwg statistic examines the variance of an observed distribution
relative to the expected variance of some null distribution (some
distribution where there’s no agreement). See also DeRue et al. (2010,
<https://doi.org/10.1111/j.1744-6570.2009.01161.x>)

``` r
# Calculate within-group agreement (rwg). I calculate this per team per measurement occasion.
# ranvar = (A^2-1)/12 with A = measurement options, so (7^2-1)/12 = 4
d2 <- d %>%
  group_by(m) %>%
  summarise(wga = list(rwg(s.pas, grpid = team, ranvar = 4)))
              
d3 <- d2 %>%
  unnest("wga") %>% 
  rename(team = grpid)

d4 <- d %>%
  left_join(d3, by = c("team", "m"))

# Average within-group agreement per day
d4 <- d4 %>%
  group_by(team, day) %>%
  mutate(rwg.d = mean(rwg))
```

Predicting daily within-group agreement over the course of the week

``` r
# Random intercept
model_3 <- lme(fixed = rwg.d ~ day.new,
                   random = ~1 | team/pid, 
                   data = d4, 
                   na.action = na.omit)

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##         AIC       BIC   logLik
    ##   -32363.73 -32319.66 16186.87
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.2285538
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept)  Residual
    ## StdDev: 4.59304e-06 0.1730016
    ## 
    ## Fixed effects:  rwg.d ~ day.new 
    ##                 Value   Std.Error    DF   t-value p-value
    ## (Intercept) 0.5639083 0.018439779 48900 30.581078  0.0000
    ## day.new     0.0009867 0.000401824 48900  2.455468  0.0141
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.084
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.8913817 -0.3331621  0.1245917  0.5052418  4.2579222 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slopes for team and pid
model_3b <- lme(fixed = rwg.d ~ day.new,
                    random = list(team = ~day.new, pid = ~day.new), 
                   data = d4, 
                   na.action = na.omit)

summary(model_3b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##         AIC       BIC   logLik
    ##   -34215.22 -34135.89 17116.61
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.23177429 (Intr)
    ## day.new     0.01922962 -0.203
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 3.929559e-06 (Intr)
    ## day.new     9.860155e-07 -0.001
    ## Residual    1.690570e-01       
    ## 
    ## Fixed effects:  rwg.d ~ day.new 
    ##                 Value   Std.Error    DF   t-value p-value
    ## (Intercept) 0.5661935 0.018695468 48900 30.285068  0.0000
    ## day.new     0.0003924 0.001595098 48900  0.245978  0.8057
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.216
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.8117045 -0.3434901  0.1122634  0.4875480  4.3323239 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Check likelihoods
model_3r <- lme(fixed = rwg.d ~ day.new,
                   random = ~1 | team/pid, 
                   data = d4, 
                   na.action = na.omit, method = "ML")

model_3br <- lme(fixed = rwg.d ~ day.new,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d4, 
                   na.action = na.omit, method = "ML")

anova(model_3r, model_3br)
```

    ##           Model df       AIC       BIC   logLik   Test  L.Ratio p-value
    ## model_3r      1  5 -32383.69 -32339.62 16196.85                        
    ## model_3br     2  9 -34232.44 -34153.11 17125.22 1 vs 2 1856.747  <.0001

``` r
# Second model is better. In this model there is no change in daily within-group agreement over the course of the week.
```

Predicting within-group agreement per measurement (!) over the course of
the week

``` r
# Random intercept
model_3c <- lme(fixed = rwg ~ day.new,
                   random = ~1 | team/pid, 
                   data = d4, 
                   na.action = na.omit)

summary(model_3c)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##        AIC      BIC    logLik
    ##   4808.363 4852.434 -2399.181
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.2283165
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)  Residual
    ## StdDev: 6.933171e-06 0.2516945
    ## 
    ## Fixed effects:  rwg ~ day.new 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept) 0.5639024 0.01851192 48900 30.461583  0.0000
    ## day.new     0.0009867 0.00058460 48900  1.687782  0.0915
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.122
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.58875805 -0.49873978  0.09681212  0.55817761  3.56750262 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Random slopes for team and pid
model_3d <- lme(fixed = rwg ~ day.new,
                    random = list(team = ~day.new, pid = ~day.new), 
                   data = d4, 
                   na.action = na.omit)

summary(model_3d)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##        AIC      BIC    logLik
    ##   4139.953 4219.282 -2060.977
    ## 
    ## Random effects:
    ##  Formula: ~day.new | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.23049758 (Intr)
    ## day.new     0.01842751 -0.184
    ## 
    ##  Formula: ~day.new | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 2.883111e-06 (Intr)
    ## day.new     1.481540e-06 -0.001
    ## Residual    2.492098e-01       
    ## 
    ## Fixed effects:  rwg ~ day.new 
    ##                 Value   Std.Error    DF   t-value p-value
    ## (Intercept) 0.5659768 0.018685677 48900 30.289341  0.0000
    ## day.new     0.0004466 0.001592105 48900  0.280517  0.7791
    ##  Correlation: 
    ##         (Intr)
    ## day.new -0.214
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.60621822 -0.49849702  0.09441832  0.56443709  3.58449578 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Check likelihoods
model_3cr <- lme(fixed = rwg ~ day.new,
                   random = ~1 | team/pid, 
                   data = d4, 
                   na.action = na.omit, method = "ML")

model_3dr <- lme(fixed = rwg ~ day.new,
                   random = list(team = ~day.new, pid = ~day.new), 
                   data = d4, 
                   na.action = na.omit, method = "ML")

anova(model_3cr, model_3dr)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_3cr     1  5 4789.153 4833.224 -2389.576                        
    ## model_3dr     2  9 4122.730 4202.059 -2052.365 1 vs 2 674.4228  <.0001

``` r
# Second model is better. No change
```

> SUMMARY: there seems to be a very slight decrease in passion
> variability within teams over the course of the week. Meaning that
> team members become slightly more similar in their passion levels as
> the week goes on. This is true when looking at daily passion levels
> and passion at each measurement, and while examining the SD while
> controlling for the mean as well as using the relative SD.
