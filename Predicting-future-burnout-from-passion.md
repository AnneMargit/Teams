Predicting future burnout from passion
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
```

Create variable “post” that indicates if measurement occurred after
maximum team-level passion (team average) was reached (post = 1 if
measurement is after, 0 = measurement is before)

``` r
load(file="d.Rdata")

d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(m = 1:n()) %>%
  ungroup()

d$team <- as.factor(d$team)
d$pid <- as.factor(d$pid)

d <- d %>%
  filter(!is.na(s.pas))

d <- d %>%
  group_by(team, m) %>%
  mutate(sd.pas = sd(s.pas),
         mean.pas = mean(s.pas)) %>%
  ungroup()

# max = 0 if mean.pas = maximum
d2 <- d %>%
  group_by(team) %>%
  mutate(max = ifelse(mean.pas == max(mean.pas), 0, 1))

# First measurement on which maximum passion was reached
d3 <- d2 %>%
  group_by(team) %>%
  arrange(max, m) %>%
  slice(1) %>%
  ungroup()

d3 <- d3 %>%
  mutate(firstm = m) %>%
  select(pid, m, firstm)

# Merge with original data
d4 <- d2 %>%
  left_join(d3, by = c("pid", "m"))

d5 <- d4 %>%
  group_by(team) %>%
  fill(firstm, .direction = "downup")

# post = 1 if measurement (m) is after the measurement on which maximum average passion was reached
d6 <- d5 %>%
  group_by(team) %>%
  mutate(post = ifelse(m > firstm, 1, 0)) 

d7 <- d6 %>%
  select(c(pid, m, post))

# Join with original data
d8 <- d %>%
  left_join(d7, by = c("pid", "m"))

d <- d8

d$post <- as.factor(d$post)

d<- d%>%
  dplyr::rename(team = team.x)
```

Centering s.pas on team level

``` r
d.c <- d %>%
  mutate(s.pas.dev = s.pas -mean.pas)
```

Create lead (= next moment) variable of burnout

``` r
d.c <- d.c[with(d.c, order(day, type, pid)),]

d.c <- d.c %>%
  group_by(pid) %>%
  mutate(s.b2.lead = lead(s.b2, n=1L))
```

## Predicting burn-out from passion

Multilevel model predicting burnout at next measurement occasion from
current passion level –> passion as deviation from team-level mean. This
answers the question: do I feel more/less burnout after if I’m more/less
passionate than my team? Multilevel model with random intercepts.

``` r
model_1 <- lme(fixed = s.b2.lead ~ s.pas.dev,
                   random = ~1 | team/pid, 
                   data = d.c, 
                   na.action = na.omit,
                   control = lmeControl(opt = "optim"))

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.c 
    ##        AIC      BIC    logLik
    ##   176720.1 176764.1 -88355.05
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.5127796
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7924147 1.435007
    ## 
    ## Fixed effects:  s.b2.lead ~ s.pas.dev 
    ##                  Value  Std.Error    DF   t-value p-value
    ## (Intercept)  2.9113389 0.05012628 48067  58.08009       0
    ## s.pas.dev   -0.1406428 0.00613756 48067 -22.91508       0
    ##  Correlation: 
    ##           (Intr)
    ## s.pas.dev 0     
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -3.5515621 -0.6101505 -0.1120993  0.4152816  4.0785599 
    ## 
    ## Number of Observations: 48897
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

Include average team-level passion

``` r
model_2 <- lme(fixed = s.b2.lead ~ mean.pas + s.pas.dev,
                   random = list(team = ~1, pid = ~1), 
                   data = d.c, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.c 
    ##        AIC      BIC    logLik
    ##   174315.3 174368.1 -87151.65
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3433713
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7937673 1.400465
    ## 
    ## Fixed effects:  s.b2.lead ~ mean.pas + s.pas.dev 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.875201 0.05585745 48066  87.27932       0
    ## mean.pas    -0.394090 0.00789424 48066 -49.92124       0
    ## s.pas.dev   -0.140497 0.00599313 48066 -23.44307       0
    ##  Correlation: 
    ##           (Intr) men.ps
    ## mean.pas  -0.704       
    ## s.pas.dev  0.000  0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.68547912 -0.61329336 -0.09641342  0.46280529  4.26556374 
    ## 
    ## Number of Observations: 48897
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
anova(model_1, model_2)
```

    ## Warning in anova.lme(model_1, model_2): fitted objects with different fixed
    ## effects. REML comparisons are not meaningful.

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_1     1  5 176720.1 176764.1 -88355.05                        
    ## model_2     2  6 174315.3 174368.1 -87151.65 1 vs 2 2406.785  <.0001

Does this depend on whether it occurs before or after a peak in
team-level passion?

``` r
model_3 <- lme(fixed = s.b2.lead ~ mean.pas + s.pas.dev + post + s.pas.dev*post,
                   random = list(team = ~1, pid = ~1), 
                   data = d.c, 
                   na.action = na.omit)

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.c 
    ##        AIC      BIC    logLik
    ##   174324.1 174394.5 -87154.06
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3438158
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.7930404 1.400387
    ## 
    ## Fixed effects:  s.b2.lead ~ mean.pas + s.pas.dev + post + s.pas.dev * post 
    ##                     Value  Std.Error    DF   t-value p-value
    ## (Intercept)      4.889014 0.05750585 48064  85.01768  0.0000
    ## mean.pas        -0.394357 0.00789828 48064 -49.92950  0.0000
    ## s.pas.dev       -0.162620 0.01005870 48064 -16.16706  0.0000
    ## post1           -0.017678 0.01746276 48064  -1.01233  0.3114
    ## s.pas.dev:post1  0.031797 0.01161472 48064   2.73764  0.0062
    ##  Correlation: 
    ##                 (Intr) men.ps s.ps.d post1 
    ## mean.pas        -0.692                     
    ## s.pas.dev        0.000  0.000              
    ## post1           -0.237  0.034  0.000       
    ## s.pas.dev:post1  0.000  0.000 -0.803  0.000
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.68901978 -0.61379876 -0.09688404  0.46354369  4.25095409 
    ## 
    ## Number of Observations: 48897
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
anova(model_2, model_3)
```

    ## Warning in anova.lme(model_2, model_3): fitted objects with different fixed
    ## effects. REML comparisons are not meaningful.

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_2     1  6 174315.3 174368.1 -87151.65                        
    ## model_3     2  8 174324.1 174394.5 -87154.06 1 vs 2 4.812283  0.0902

Including the post-peak dummy does not change the model fit so we should
leave these predictors out.

Model 2 + random slopes for s.pas.dev at id level.

``` r
model_4 <- lme(fixed = s.b2.lead ~ mean.pas + s.pas.dev,
                   random = list(team = ~1, pid = ~s.pas.dev),
                   data = d.c, 
                   na.action = na.omit)

summary(model_4)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.c 
    ##        AIC      BIC    logLik
    ##   171442.3 171512.6 -85713.13
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3387765
    ## 
    ##  Formula: ~s.pas.dev | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.7487607 (Intr)
    ## s.pas.dev   0.4205495 0.094 
    ## Residual    1.3400127       
    ## 
    ## Fixed effects:  s.b2.lead ~ mean.pas + s.pas.dev 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.990644 0.05552713 48066  89.87757       0
    ## mean.pas    -0.411782 0.00801301 48066 -51.38911       0
    ## s.pas.dev   -0.143821 0.01653805 48066  -8.69636       0
    ##  Correlation: 
    ##           (Intr) men.ps
    ## mean.pas  -0.721       
    ## s.pas.dev  0.038 -0.003
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.65992732 -0.58564178 -0.07692764  0.45247301  4.44933404 
    ## 
    ## Number of Observations: 48897
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
anova(model_2, model_4)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_2     1  6 174315.3 174368.1 -87151.65                        
    ## model_4     2  8 171442.3 171512.6 -85713.13 1 vs 2 2877.049  <.0001

Model with random slopes for s.pas.dev at id level and team level.

``` r
model_5 <- lme(fixed = s.b2.lead ~ mean.pas + s.pas.dev,
                   random = list(team = ~s.pas.dev, pid = ~s.pas.dev),
                   data = d.c, 
                   na.action = na.omit)

summary(model_5)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d.c 
    ##        AIC      BIC    logLik
    ##   171433.4 171521.4 -85706.71
    ## 
    ## Random effects:
    ##  Formula: ~s.pas.dev | team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.3343724 (Intr)
    ## s.pas.dev   0.1355767 -0.269
    ## 
    ##  Formula: ~s.pas.dev | pid %in% team
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.7497331 (Intr)
    ## s.pas.dev   0.3980581 0.118 
    ## Residual    1.3400214       
    ## 
    ## Fixed effects:  s.b2.lead ~ mean.pas + s.pas.dev 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.989361 0.05533975 48066  90.15871       0
    ## mean.pas    -0.411550 0.00800614 48066 -51.40427       0
    ## s.pas.dev   -0.144304 0.01935333 48066  -7.45629       0
    ##  Correlation: 
    ##           (Intr) men.ps
    ## mean.pas  -0.723       
    ## s.pas.dev -0.036 -0.002
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.65679908 -0.58400277 -0.07704611  0.45116511  4.43238802 
    ## 
    ## Number of Observations: 48897
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
anova(model_4, model_5)
```

    ##         Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_4     1  8 171442.3 171512.6 -85713.13                        
    ## model_5     2 10 171433.4 171521.4 -85706.71 1 vs 2 12.82677  0.0016

Model with random slope at id-level seems better (model 4).

> Results suggest that a positive deviation from average team passion
> levels predicts lower burnout at the next measurement occasion. Also,
> average team level passion is associated with lower burnout at the
> next measurement.

Plot of predicted values

``` r
eff <- effect("s.pas.dev", model_4)

plotje <- ggplot(as.data.frame(eff),
                 aes(s.pas.dev, fit)) +
  geom_line() +
  geom_errorbar(aes(ymin = fit-se, ymax = fit+se), width = 1)
```
