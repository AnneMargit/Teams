Predicting passion variance over time #2
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
```

## Prep

Create measurement indicator (m) and calculate within-team average
(mean.pas) and standard deviation (sd.pas) of passion (on each
measurement occasion):

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
```

We want to look at SD variability across days, so I recode the daytime
indicator (type) into 1,2,3

``` r
d$type <- as.factor(d$type)
levels(d$type)
```

    ## [1] "a" "e" "m"

``` r
# They are ordered afternoon, evening, morning

levels(d$type) <- c("2", "3", "1")

d$type <- as.numeric(d$type)
```

## Predicting within-day SD variability

Three-level multilevel model (teams/participants/day), although maybe it
should be a four-level model?

``` r
# Random intercept (+ correcting for the mean)
model_1 <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = ~1 | team/pid/day, 
                   data = d, 
                   na.action = na.omit)

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   54495.07 54556.77 -27240.54
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3455042
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept)
    ## StdDev: 1.53232e-05
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept)  Residual
    ## StdDev:    0.219881 0.3676773
    ## 
    ## Fixed effects:  sd.pas ~ type + mean.pas 
    ##                  Value   Std.Error    DF   t-value p-value
    ## (Intercept)  2.4579768 0.031024045 33148  79.22812       0
    ## type         0.0246568 0.002019627 33148  12.20857       0
    ## mean.pas    -0.2540614 0.002628961 33148 -96.63947       0
    ##  Correlation: 
    ##          (Intr) type  
    ## type     -0.125       
    ## mean.pas -0.421 -0.012
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.09882457 -0.45247162 -0.02391217  0.41899383  4.84136116 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
# Random slope for type on team and participant level
model_1b <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d, 
                   na.action = na.omit)

summary(model_1b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   41519.75 41616.71 -20748.87
    ## 
    ## Random effects:
    ##  Formula: ~type | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.5971190 (Intr)
    ## type        0.2249437 -0.808
    ## 
    ##  Formula: ~type | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 8.337012e-06 (Intr)
    ## type        7.400894e-06 -0.001
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept)  Residual
    ## StdDev:   0.2457847 0.3011633
    ## 
    ## Fixed effects:  sd.pas ~ type + mean.pas 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.2632882 0.04977628 33148  45.46921  0.0000
    ## type         0.0284271 0.01814581 33148   1.56659  0.1172
    ## mean.pas    -0.2164943 0.00254267 33148 -85.14464  0.0000
    ##  Correlation: 
    ##          (Intr) type  
    ## type     -0.781       
    ## mean.pas -0.254  0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.74159649 -0.40670229 -0.01991272  0.38407097  5.61426111 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
# Compare likelihoods (using ML instead of REML, otherwise likelihoods are not comparable)
model_1r <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = ~1 | team/pid/day, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_1br <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_1r, model_1br)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_1r      1  7 54469.13 54530.83 -27227.56                        
    ## model_1br     2 11 41498.16 41595.12 -20738.08 1 vs 2 12978.97  <.0001

``` r
# Second model including random slopes for team and pid is better
# Adding random slope for day 

# Random slope for day on team and participant and day level
model_1c <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = list(team = ~type, pid = ~type, day = ~type), 
                   data = d, 
                   na.action = na.omit)

summary(model_1c)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   41523.75 41638.33 -20748.87
    ## 
    ## Random effects:
    ##  Formula: ~type | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.5971185 (Intr)
    ## type        0.2249435 -0.808
    ## 
    ##  Formula: ~type | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 6.286841e-06 (Intr)
    ## type        3.989311e-06 0     
    ## 
    ##  Formula: ~type | day %in% pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 2.457847e-01 (Intr)
    ## type        1.299842e-05 0     
    ## Residual    3.011633e-01       
    ## 
    ## Fixed effects:  sd.pas ~ type + mean.pas 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.2632882 0.04977624 33148  45.46925  0.0000
    ## type         0.0284271 0.01814579 33148   1.56659  0.1172
    ## mean.pas    -0.2164943 0.00254267 33148 -85.14464  0.0000
    ##  Correlation: 
    ##          (Intr) type  
    ## type     -0.781       
    ## mean.pas -0.254  0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.74159642 -0.40670229 -0.01991273  0.38407096  5.61426103 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
model_1cr <- lme(fixed = sd.pas ~ type + mean.pas,
                   random = list(team = ~type, pid = ~type, day = ~type), 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_1br, model_1cr)
```

    ##           Model df      AIC      BIC    logLik   Test      L.Ratio p-value
    ## model_1br     1 11 41498.16 41595.12 -20738.08                            
    ## model_1cr     2 13 41502.16 41616.75 -20738.08 1 vs 2 8.215953e-06       1

``` r
# Best model is model with random slopes for team and participants. In this model there is no significant change in SD over the course of the day
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

Predicting within-team passion variance over time using the relative sd.

``` r
# Random intercept
model_2 <- lme(fixed = rl.sd ~ type,
                   random = ~1 | team/pid/day, 
                   data = d2, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##         AIC       BIC   logLik
    ##   -42881.16 -42828.28 21446.58
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.1391977
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)
    ## StdDev: 5.817423e-06
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept)  Residual
    ## StdDev:  0.09080745 0.1351697
    ## 
    ## Fixed effects:  rl.sd ~ type 
    ##                 Value   Std.Error    DF  t-value p-value
    ## (Intercept) 0.4655189 0.011318470 33109 41.12914       0
    ## type        0.0052814 0.000742959 33109  7.10862       0
    ##  Correlation: 
    ##      (Intr)
    ## type -0.131
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.17295884 -0.43676625 -0.04619342  0.42499524  4.54520768 
    ## 
    ## Number of Observations: 49685
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16575

``` r
# Random slopes for team and pid
model_2b <- lme(fixed = rl.sd ~ type,
                    random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d2, 
                   na.action = na.omit)

summary(model_2b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##         AIC       BIC   logLik
    ##   -54037.17 -53949.04 27028.59
    ## 
    ## Random effects:
    ##  Formula: ~type | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev     Corr  
    ## (Intercept) 0.21784455 (Intr)
    ## type        0.07636468 -0.772
    ## 
    ##  Formula: ~type | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 3.743016e-06 (Intr)
    ## type        2.255240e-06 -0.001
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept) Residual
    ## StdDev:  0.09996367 0.113146
    ## 
    ## Fixed effects:  rl.sd ~ type 
    ##                 Value   Std.Error    DF   t-value p-value
    ## (Intercept) 0.4623186 0.017568529 33109 26.315159  0.0000
    ## type        0.0068213 0.006166184 33109  1.106243  0.2686
    ##  Correlation: 
    ##      (Intr)
    ## type -0.772
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.99651592 -0.38198320 -0.02918622  0.39626302  4.65092868 
    ## 
    ## Number of Observations: 49685
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16575

``` r
# Check likelihoods
model_2r <- lme(fixed = rl.sd ~ type,
                   random = ~1 | team/pid/day, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

model_2br <- lme(fixed = rl.sd ~ type,
                   random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d2, 
                   na.action = na.omit, method = "ML")

anova(model_2r, model_2br)
```

    ##           Model df       AIC       BIC   logLik   Test  L.Ratio p-value
    ## model_2r      1  6 -42900.88 -42848.00 21456.44                        
    ## model_2br     2 10 -54052.67 -53964.54 27036.33 1 vs 2 11159.79  <.0001

``` r
# Second model is better. In this model there is also no change in SD over the days
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
```

Predicting the within-group agreement over the day

``` r
# Random intercept
model_3 <- lme(fixed = rwg ~ type,
                   random = ~1 | team/pid/day, 
                   data = d4, 
                   na.action = na.omit)

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##        AIC      BIC    logLik
    ##   2736.002 2788.888 -1362.001
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.2281269
    ## 
    ##  Formula: ~1 | pid %in% team
    ##          (Intercept)
    ## StdDev: 1.049471e-05
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept)  Residual
    ## StdDev:   0.1161437 0.2233983
    ## 
    ## Fixed effects:  rwg ~ type 
    ##                  Value   Std.Error    DF   t-value p-value
    ## (Intercept)  0.5834362 0.018537787 33149 31.472810       0
    ## type        -0.0078703 0.001227015 33149 -6.414198       0
    ##  Correlation: 
    ##      (Intr)
    ## type -0.132
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -3.46035772 -0.39175508  0.06496097  0.44641824  3.45611574 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
# Random slopes for team and pid
model_3b <- lme(fixed = rwg ~ type,
                    random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d4, 
                   na.action = na.omit)

summary(model_3b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##         AIC      BIC   logLik
    ##   -10826.85 -10738.7 5423.424
    ## 
    ## Random effects:
    ##  Formula: ~type | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.3673536 (Intr)
    ## type        0.1367864 -0.785
    ## 
    ##  Formula: ~type | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 5.835073e-06 (Intr)
    ## type        3.506515e-06 -0.001
    ## 
    ##  Formula: ~1 | day %in% pid %in% team
    ##         (Intercept)  Residual
    ## StdDev:   0.1389905 0.1800322
    ## 
    ## Fixed effects:  rwg ~ type 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  0.5890758 0.02960645 33149 19.896874  0.0000
    ## type        -0.0106899 0.01103272 33149 -0.968928  0.3326
    ##  Correlation: 
    ##      (Intr)
    ## type -0.785
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.41436279 -0.37771964  0.04139336  0.36869252  4.07003121 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
# Random slopes for team and pid and day
model_3c <- lme(fixed = rwg ~ type,
                    random = list(team = ~type, pid = ~type, day = ~type), 
                   data = d4, 
                   na.action = na.omit)

summary(model_3c)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d4 
    ##         AIC       BIC   logLik
    ##   -10822.85 -10717.08 5423.424
    ## 
    ## Random effects:
    ##  Formula: ~type | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.3673536 (Intr)
    ## type        0.1367864 -0.785
    ## 
    ##  Formula: ~type | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 3.621596e-06 (Intr)
    ## type        1.188875e-06 0     
    ## 
    ##  Formula: ~type | day %in% pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev       Corr  
    ## (Intercept) 1.389905e-01 (Intr)
    ## type        8.992561e-06 0     
    ## Residual    1.800322e-01       
    ## 
    ## Fixed effects:  rwg ~ type 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  0.5890758 0.02960645 33149 19.896874  0.0000
    ## type        -0.0106899 0.01103272 33149 -0.968927  0.3326
    ##  Correlation: 
    ##      (Intr)
    ## type -0.785
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.41436276 -0.37771966  0.04139336  0.36869253  4.07003115 
    ## 
    ## Number of Observations: 49730
    ## Number of Groups: 
    ##                   team          pid %in% team day %in% pid %in% team 
    ##                    155                    829                  16580

``` r
# Check likelihoods
model_3r <- lme(fixed = rwg ~ type,
                   random = ~1 | team/pid/day, 
                   data = d4, 
                   na.action = na.omit, method = "ML")

model_3br <- lme(fixed = rwg ~ type,
                   random = list(team = ~type, pid = ~type, day = ~1), 
                   data = d4, 
                   na.action = na.omit, method = "ML")

model_3cr <- lme(fixed = rwg ~ type,
                    random = list(team = ~type, pid = ~type, day = ~type), 
                   data = d4, 
                   na.action = na.omit, method = "ML")

anova(model_3r, model_3br, model_3cr)
```

    ##           Model df        AIC       BIC    logLik   Test  L.Ratio p-value
    ## model_3r      1  6   2718.274   2771.16 -1353.137                        
    ## model_3br     2 10 -10840.190 -10752.05  5430.095 1 vs 2 13566.46  <.0001
    ## model_3cr     3 12 -10836.190 -10730.42  5430.095 2 vs 3     0.00       1

``` r
# Second model is better. In this model there is no change in within-group agreement over the days
```
