CRRC passion predicted by difference in own experience - team perception
================

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

``` r
library(psych)
library(nlme)
library(ggplot2)
library(effects)
library(correlation)
```

    ## Warning: package 'correlation' was built under R version 4.0.5

``` r
load("/Volumes/Anne/Harvard/Teams/Teams/d.Rdata")
```

### How does the difference between self- and other report stress at current moment predict passion for work at next moment?

Create lead variable for passion and difference scores (i.e. next moment
- current moment)

``` r
d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(s.pas.lead = lead(s.pas, n=1L)) %>%
  mutate(s.pas.diff = s.pas.lead - s.pas) %>%
  ungroup()
```

Stress perceived by team members (o.em1)

``` r
d <- d %>%
  group_by(pid, day, type) %>%
    mutate(stress.av = mean(c(o.em1_1, o.em1_2, o.em1_3, o.em1_4, o.em1_5, o.em1_6, o.em1_7), na.rm=T))

d$stress.av[is.nan(d$stress.av)]<-NA
```

Create stress_av lead, this leads the stress_av variable with 1
measurement occasion because other report about the morning is collected
in the afternoon, and other report about the afternoon is collected in
the evening.

``` r
d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(stress.av.lead = lead(stress.av, n=1L)) %>%
  ungroup()
```

Calculate difference between self-reported stress and other reported
stress

``` r
# If stress.diff is positive, this means that I am more stressed than team members think I am
d <- d %>%
  mutate(stress.diff = s.em1 - stress.av, na.rm=T)
```

Correlation between self- and other reported stress at time t and
passion for work at time t+1

``` r
# Self-report
d %>%
  select(s.em1, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1 | Parameter2 |     r |         95% CI | t(48889) |         p
    ## -----------------------------------------------------------------------
    ## s.em1      | s.pas.lead | -0.27 | [-0.28, -0.26] |   -62.44 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 48891

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=s.em1)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 849 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 849 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Other report
d %>%
  select(stress.av.lead, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1     | Parameter2 |     r |         95% CI | t(32316) |         p
    ## ---------------------------------------------------------------------------
    ## stress.av.lead | s.pas.lead | -0.38 | [-0.39, -0.37] |   -74.27 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 32318

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=stress.av.lead)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 17422 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 17422 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
# Difference between self-report and other report
d %>%
  select(stress.diff, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1  | Parameter2 |     r |         95% CI | t(33139) |         p
    ## ------------------------------------------------------------------------
    ## stress.diff | s.pas.lead | -0.05 | [-0.06, -0.04] |    -9.66 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 33141

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=stress.diff)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 16599 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 16599 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->
Multilevel model, persons nested in teams, passion at time t+1 predicted
by the self-other report discrepancy

``` r
# Random intercept for team and id
model1 <- lme(fixed = s.pas.lead ~ stress.diff,
              random = ~1 | tid/pid,
              data = d,
              na.action = na.omit)

summary(model1)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##      AIC      BIC    logLik
    ##   115047 115089.1 -57518.52
    ## 
    ## Random effects:
    ##  Formula: ~1 | tid
    ##          (Intercept)
    ## StdDev: 0.0003049099
    ## 
    ##  Formula: ~1 | pid %in% tid
    ##         (Intercept) Residual
    ## StdDev:   0.9224753   1.3214
    ## 
    ## Fixed effects: s.pas.lead ~ stress.diff 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.964050 0.03285406 32311 151.09397       0
    ## stress.diff -0.037291 0.00570121 32311  -6.54094       0
    ##  Correlation: 
    ##             (Intr)
    ## stress.diff 0.013 
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.3080110 -0.3982631  0.0969506  0.5596201  3.7969459 
    ## 
    ## Number of Observations: 33141
    ## Number of Groups: 
    ##          tid pid %in% tid 
    ##            7          829

``` r
# Random slopes for team and id
model2 <- lme(fixed = s.pas.lead ~ stress.diff,
              random = ~ stress.diff | tid/pid,
              data = d,
              na.action = na.omit)

summary(model2)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC      BIC    logLik
    ##   110396.4 110472.1 -55189.19
    ## 
    ## Random effects:
    ##  Formula: ~stress.diff | tid
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev      Corr  
    ## (Intercept) 0.007508034 (Intr)
    ## stress.diff 0.015701365 0.474 
    ## 
    ##  Formula: ~stress.diff | pid %in% tid
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.8425525 (Intr)
    ## stress.diff 0.4665675 0.07  
    ## Residual    1.1998005       
    ## 
    ## Fixed effects: s.pas.lead ~ stress.diff 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  4.867788 0.03062677 32311 158.93901   0e+00
    ## stress.diff -0.063901 0.01882148 32311  -3.39513   7e-04
    ##  Correlation: 
    ##             (Intr)
    ## stress.diff 0.09  
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.48183427 -0.38356929  0.06318773  0.53161172  5.42633291 
    ## 
    ## Number of Observations: 33141
    ## Number of Groups: 
    ##          tid pid %in% tid 
    ##            7          829

### How does the difference between self-reported happiness and team-reported happiness associate with passion over time?

Happiness perceived by team members (o.em2)

``` r
d <- d %>%
  group_by(pid, day, type) %>%
    mutate(hap.av = mean(c(o.em2_1, o.em2_2, o.em2_3, o.em2_4, o.em2_5, o.em2_6, o.em2_7), na.rm=T))

d$hap.av[is.nan(d$hap.av)]<-NA
```

Create hap.av lead (same thing as for stress.av).

``` r
d <- d[with(d, order(pid, day, type)),]

d <- d %>%
  group_by(pid) %>%
  mutate(hap.av.lead = lead(hap.av, n=1L)) %>%
  ungroup()
```

Calculate difference between self-reported happiness and other reported
happiness

``` r
# If hap.diff is positive, this means that I am happier than team members think I am
d <- d %>%
  mutate(hap.diff = s.em2 - hap.av, na.rm=T)
```

Correlation between self- and other reported happiness at time t and
passion for work at time t+1

``` r
# Self-report
d %>%
  select(s.em2, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1 | Parameter2 |    r |       95% CI | t(48889) |         p
    ## --------------------------------------------------------------------
    ## s.em2      | s.pas.lead | 0.27 | [0.26, 0.28] |    61.47 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 48891

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=s.em2)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 849 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 849 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Other report
d %>%
  select(hap.av.lead, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1  | Parameter2 |    r |       95% CI | t(32316) |         p
    ## ---------------------------------------------------------------------
    ## hap.av.lead | s.pas.lead | 0.38 | [0.37, 0.39] |    74.66 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 32318

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=hap.av.lead)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 17422 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 17422 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# Difference between self-report and other report
d %>%
  select(hap.diff, s.pas.lead) %>%
  correlation()
```

    ## # Correlation Matrix (pearson-method)
    ## 
    ## Parameter1 | Parameter2 |    r |       95% CI | t(33139) |         p
    ## --------------------------------------------------------------------
    ## hap.diff   | s.pas.lead | 0.04 | [0.03, 0.06] |     8.20 | < .001***
    ## 
    ## p-value adjustment method: Holm (1979)
    ## Observations: 33141

``` r
d %>%
  ggplot(aes(x=s.pas.lead, y=hap.diff)) + 
  geom_jitter() +
  geom_smooth(method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 16599 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 16599 rows containing missing values (geom_point).

![](CRRC-passion-predicted-by-difference-in-stress-experience---perception_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

Multilevel model, persons nested in teams, passion at time t+1 predicted
by the self-other report discrepancy

``` r
# Random intercept for team and id
model3 <- lme(fixed = s.pas.lead ~ hap.diff,
              random = ~1 | tid/pid,
              data = d,
              na.action = na.omit)

summary(model3)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC    BIC    logLik
    ##   115080.9 115123 -57535.47
    ## 
    ## Random effects:
    ##  Formula: ~1 | tid
    ##          (Intercept)
    ## StdDev: 0.0002981076
    ## 
    ##  Formula: ~1 | pid %in% tid
    ##         (Intercept) Residual
    ## StdDev:    0.923409 1.322058
    ## 
    ## Fixed effects: s.pas.lead ~ hap.diff 
    ##                Value  Std.Error    DF  t-value p-value
    ## (Intercept) 4.965497 0.03288701 32311 150.9866  0.0000
    ## hap.diff    0.016136 0.00538682 32311   2.9954  0.0027
    ##  Correlation: 
    ##          (Intr)
    ## hap.diff -0.015
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.30092438 -0.39802047  0.09466825  0.55386700  3.78733101 
    ## 
    ## Number of Observations: 33141
    ## Number of Groups: 
    ##          tid pid %in% tid 
    ##            7          829

``` r
# Random slopes for team and id
model4 <- lme(fixed = s.pas.lead ~ hap.diff,
              random = ~ hap.diff | tid/pid,
              data = d,
              na.action = na.omit)

summary(model4)
```

    ## Linear mixed-effects model fit by REML
    ##  Data: d 
    ##        AIC      BIC    logLik
    ##   111431.5 111507.2 -55706.76
    ## 
    ## Random effects:
    ##  Formula: ~hap.diff | tid
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev      Corr  
    ## (Intercept) 0.001117307 (Intr)
    ## hap.diff    0.023985427 0.178 
    ## 
    ##  Formula: ~hap.diff | pid %in% tid
    ##  Structure: General positive-definite, Log-Cholesky parametrization
    ##             StdDev    Corr  
    ## (Intercept) 0.8566395 (Intr)
    ## hap.diff    0.3917868 -0.037
    ## Residual    1.2213840       
    ## 
    ## Fixed effects: s.pas.lead ~ hap.diff 
    ##                Value  Std.Error    DF   t-value p-value
    ## (Intercept) 4.883046 0.03089337 32311 158.06127  0.0000
    ## hap.diff    0.058040 0.01792409 32311   3.23812  0.0012
    ##  Correlation: 
    ##          (Intr)
    ## hap.diff -0.041
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.25988179 -0.40494384  0.07761986  0.53115834  5.01433065 
    ## 
    ## Number of Observations: 33141
    ## Number of Groups: 
    ##          tid pid %in% tid 
    ##            7          829
