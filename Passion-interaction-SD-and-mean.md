Passion interaction SD and mean
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
library(psych)
library(ggpubr)
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

## Predicting emotional exhaustion

Predicting emotional exhaustion (evening) from the interaction between
daily team passion mean and SD:

``` r
model_1 <- lme(fixed = s.ee ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_1)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   56610.17 56656.46 -28299.09
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.1992932
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.218432 1.235588
    ## 
    ## Fixed effects:  s.ee ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  5.455997 0.10689195 15748  51.04216  0.0000
    ## sd.pas.d    -0.033096 0.03336125 15748  -0.99204  0.3212
    ## mean.pas.d  -0.420181 0.01469299 15748 -28.59741  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.632       
    ## mean.pas.d -0.822  0.346
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.67644092 -0.44069018 -0.04712748  0.37260524  4.85005559 
    ## 
    ## Number of Observations: 16579
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_1b <- lme(fixed = s.ee ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_1b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC     BIC   logLik
    ##   56592.79 56646.8 -28289.4
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.1883681
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.217963 1.234753
    ## 
    ## Fixed effects:  s.ee ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)          4.737114 0.17922601 15747 26.430953       0
    ## sd.pas.d             0.548611 0.12117944 15747  4.527262       0
    ## mean.pas.d          -0.270015 0.03347544 15747 -8.066063       0
    ## sd.pas.d:mean.pas.d -0.126147 0.02526774 15747 -4.992418       0
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.876              
    ## mean.pas.d          -0.937  0.906       
    ## sd.pas.d:mean.pas.d  0.804 -0.961 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.37145477 -0.44096776 -0.04844734  0.37186703  4.63794931 
    ## 
    ## Number of Observations: 16579
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_1r <- lme(fixed = s.ee ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_1br <- lme(fixed = s.ee ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_1r, model_1br)
```

    ##           Model df      AIC      BIC    logLik   Test L.Ratio p-value
    ## model_1r      1  6 56594.17 56640.46 -28291.08                       
    ## model_1br     2  7 56571.25 56625.26 -28278.63 1 vs 2 24.9135  <.0001

Plotting the interaction between passion SD and mean for higher and
lower levels of the mean (+ and -2 SD)

``` r
describe(d$mean.pas.d)
```

    ##    vars     n mean   sd median trimmed  mad  min max range  skew kurtosis se
    ## X1    1 49730 4.98 0.94   5.13    5.08 0.79 1.25   7  5.75 -1.02     1.23  0

``` r
#sd = 0.94 --> 2*0.94 = 1.88

eff <- effect("sd.pas.d:mean.pas.d", xlevels = list(mean.pas.d = c(-1.88, 1.88)), model_1b)

effdata <- as.data.frame(eff)

effdata$mean.pas.d <- as.factor(effdata$mean.pas.d)

plot <- ggplot(effdata, aes(x = sd.pas.d, y = fit, color = mean.pas.d, group = mean.pas.d)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=mean.pas.d),alpha=0.3) +
  labs(title = "Emotional exhaustion from team passion", x= "Within-team passion SD", y="Emotional exhaustion (individual)", color="Team passion intensity", fill="Team passion intensity") + theme_classic() + theme(text=element_text(size=12)) + 
  scale_fill_discrete(labels = c("mean -2SD", "mean + 2SD")) +   scale_color_discrete(labels = c("mean -2SD", "mean + 2SD"))
```

``` r
plot
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
## Next morning stress (stress.m) and burnout (burnout.m)

``` r
d <- d %>%
  mutate(stress.m = lead(s.em1, n=1L),
         burnout.m = lead(s.b2, 1L))

# Select only evening measurements (doesn't matter for average team passion/sd but does for stress)

d2 <- d %>%
  filter(type == "e")
```

Models for stress:

``` r
model_2 <- lme(fixed = stress.m ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_2)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##        AIC      BIC    logLik
    ##   54235.75 54282.05 -27111.88
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3754818
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9269408 1.158934
    ## 
    ## Fixed effects:  stress.m ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  6.076507 0.10107451 15749  60.11908  0.0000
    ## sd.pas.d     0.116246 0.03135899 15749   3.70695  0.0002
    ## mean.pas.d  -0.689737 0.01379365 15749 -50.00390  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.627       
    ## mean.pas.d -0.815  0.345
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -6.80519195 -0.48002085 -0.04697026  0.34614456  4.68695410 
    ## 
    ## Number of Observations: 16580
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_2b <- lme(fixed = stress.m ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_2b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##     AIC      BIC logLik
    ##   54240 54294.01 -27113
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3734523
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9269506 1.158872
    ## 
    ## Fixed effects:  stress.m ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)          5.827051 0.16883513 15748  34.51326  0.0000
    ## sd.pas.d             0.318066 0.11386714 15748   2.79331  0.0052
    ## mean.pas.d          -0.637596 0.03144526 15748 -20.27639  0.0000
    ## sd.pas.d:mean.pas.d -0.043800 0.02374473 15748  -1.84461  0.0651
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.874              
    ## mean.pas.d          -0.934  0.906       
    ## sd.pas.d:mean.pas.d  0.801 -0.961 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -6.69111065 -0.47960716 -0.04990245  0.34928551  4.69732547 
    ## 
    ## Number of Observations: 16580
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_2r <- lme(fixed = stress.m ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

model_2br <- lme(fixed = stress.m ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

anova(model_2r, model_2br)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_2r      1  6 54219.45 54265.75 -27103.73                        
    ## model_2br     2  7 54218.04 54272.06 -27102.02 1 vs 2 3.406845  0.0649

> Best model is model without interaction effect

Plots for stress:

``` r
eff1 <- effect(c("sd.pas.d"),  model_2)
eff2 <- effect(c("mean.pas.d"),  model_2)

effdata1 <- as.data.frame(eff1)
effdata2 <- as.data.frame(eff2)

plot1 <- ggplot(effdata1, aes(x = sd.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion SD", y="Stress (morning)") + theme_classic() + theme(text=element_text(size=12)) 

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion mean", y="Stress (morning)") + theme_classic() + theme(text=element_text(size=12)) 

library(ggpubr)

plots <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)
plots <- annotate_figure(plots, top = text_grob("Next morning stress from team passion mean and SD"))

plots
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Models for burnout

``` r
model_3 <- lme(fixed = burnout.m ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_3)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##        AIC      BIC    logLik
    ##   54826.23 54872.52 -27407.11
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3097383
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.042464 1.175795
    ## 
    ## Fixed effects:  burnout.m ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  6.382957 0.10216404 15745  62.47753  0.0000
    ## sd.pas.d     0.118665 0.03178358 15745   3.73352  0.0002
    ## mean.pas.d  -0.747157 0.01399110 15745 -53.40227  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.629       
    ## mean.pas.d -0.818  0.345
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -6.93703949 -0.46957697 -0.05974242  0.34385234  5.81815106 
    ## 
    ## Number of Observations: 16576
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_3b <- lme(fixed = burnout.m ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit)

summary(model_3b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d2 
    ##        AIC      BIC    logLik
    ##   54833.54 54887.55 -27409.77
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3103674
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.042481 1.175812
    ## 
    ## Fixed effects:  burnout.m ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)          6.458557 0.17107580 15744  37.75261  0.0000
    ## sd.pas.d             0.057526 0.11549163 15744   0.49810  0.6184
    ## mean.pas.d          -0.762953 0.03189579 15744 -23.92018  0.0000
    ## sd.pas.d:mean.pas.d  0.013265 0.02408346 15744   0.55079  0.5818
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.874              
    ## mean.pas.d          -0.935  0.906       
    ## sd.pas.d:mean.pas.d  0.802 -0.961 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -6.9710769 -0.4706300 -0.0598222  0.3448772  5.8419073 
    ## 
    ## Number of Observations: 16576
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_3r <- lme(fixed = burnout.m ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

model_3br <- lme(fixed = burnout.m ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d2, 
                   na.action = na.omit, method = "ML")

anova(model_3r, model_3br)
```

    ##           Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## model_3r      1  6 54809.97 54856.27 -27398.99                         
    ## model_3br     2  7 54811.67 54865.68 -27398.83 1 vs 2 0.3019583  0.5827

> The model without the interaction is best but there are main effects
> for the meand and SD

Plots for burnout:

``` r
eff1 <- effect(c("sd.pas.d"),  model_3)
eff2 <- effect(c("mean.pas.d"),  model_3)

effdata1 <- as.data.frame(eff1)
effdata2 <- as.data.frame(eff2)

plot1 <- ggplot(effdata1, aes(x = sd.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion SD", y="Burnout (morning)") + theme_classic() + theme(text=element_text(size=12)) 

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion mean", y="Burnout (morning)") + theme_classic() + theme(text=element_text(size=12)) 

plots <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)
plots <- annotate_figure(plots, top = text_grob("Next morning burnout from team passion mean and SD"))

plots
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Working hours

Association between daily team average passion and SD and hours worked
(s.wl2):

``` r
model_4 <- lme(fixed = s.wl2 ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_4)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   54355.89 54402.18 -27171.95
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.6568556
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.155961 1.148881
    ## 
    ## Fixed effects:  s.wl2 ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept) 12.692492 0.11251018 15747 112.81194   0.000
    ## sd.pas.d     0.093953 0.03165782 15747   2.96776   0.003
    ## mean.pas.d  -0.561014 0.01376304 15747 -40.76236   0.000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.561       
    ## mean.pas.d -0.729  0.337
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -5.2359661 -0.3609367 -0.0672366  0.2166493  6.0868546 
    ## 
    ## Number of Observations: 16578
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_4b <- lme(fixed = s.wl2 ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_4b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC   BIC    logLik
    ##   54360.99 54415 -27173.49
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.652697
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:     1.15597 1.148868
    ## 
    ## Fixed effects:  s.wl2 ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)         12.477368 0.17537933 15746  71.14503  0.0000
    ## sd.pas.d             0.268557 0.11375575 15746   2.36083  0.0182
    ## mean.pas.d          -0.516027 0.03135931 15746 -16.45529  0.0000
    ## sd.pas.d:mean.pas.d -0.037923 0.02373935 15746  -1.59748  0.1102
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.838              
    ## mean.pas.d          -0.895  0.904       
    ## sd.pas.d:mean.pas.d  0.768 -0.961 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.25418696 -0.35940491 -0.06928442  0.21522380  6.08585002 
    ## 
    ## Number of Observations: 16578
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_4r <- lme(fixed = s.wl2 ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_4br <- lme(fixed = s.wl2 ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_4r, model_4br)
```

    ##           Model df      AIC      BIC    logLik   Test  L.Ratio p-value
    ## model_4r      1  6 54340.40 54386.70 -27164.20                        
    ## model_4br     2  7 54339.84 54393.86 -27162.92 1 vs 2 2.555744  0.1099

> There is no interaction effect between SD and team average passion.
> There are significant main effects: Higher team passion SD is
> associated with more hours worked, and higher team passion average is
> associated with less (!) hours worked

Plots for working hours:

``` r
eff1 <- effect(c("sd.pas.d"),  model_4)
eff2 <- effect(c("mean.pas.d"),  model_4)

effdata1 <- as.data.frame(eff1)
effdata2 <- as.data.frame(eff2)

plot1 <- ggplot(effdata1, aes(x = sd.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion SD", y="Hours worked") + theme_classic() + theme(text=element_text(size=12)) 

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion mean", y="Hours worked") + theme_classic() + theme(text=element_text(size=12)) 

plots <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)
plots <- annotate_figure(plots, top = text_grob("Hours worked predicted from team passion mean and SD"))

plots
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

## Workload

Association between daily team average passion and SD and experienced
workload (s.wl1):

``` r
model_5 <- lme(fixed = s.wl1 ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_5)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   53593.04 53639.34 -26790.52
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.268027
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9944975  1.13349
    ## 
    ## Fixed effects:  s.wl1 ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  6.607197 0.09768486 15747  67.63788  0.0000
    ## sd.pas.d    -0.110496 0.03056089 15747  -3.61560  0.0003
    ## mean.pas.d  -0.659501 0.01347257 15747 -48.95141  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.634       
    ## mean.pas.d -0.824  0.346
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -5.5796312 -0.4161739 -0.0372206  0.3254134 42.6072724 
    ## 
    ## Number of Observations: 16578
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_5b <- lme(fixed = s.wl1 ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit)

summary(model_5b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d 
    ##        AIC      BIC    logLik
    ##   53600.73 53654.74 -26793.36
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.2680957
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9945085 1.133524
    ## 
    ## Fixed effects:  s.wl1 ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)          6.616558 0.16437582 15746  40.25262  0.0000
    ## sd.pas.d            -0.118066 0.11120400 15746  -1.06171  0.2884
    ## mean.pas.d          -0.661456 0.03072164 15746 -21.53063  0.0000
    ## sd.pas.d:mean.pas.d  0.001642 0.02318659 15746   0.07082  0.9435
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.877              
    ## mean.pas.d          -0.938  0.906       
    ## sd.pas.d:mean.pas.d  0.804 -0.961 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -5.57810971 -0.41598498 -0.03726986  0.32544604 42.60650925 
    ## 
    ## Number of Observations: 16578
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_5r <- lme(fixed = s.wl1 ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

model_5br <- lme(fixed = s.wl1 ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d, 
                   na.action = na.omit, method = "ML")

anova(model_5r, model_5br)
```

    ##           Model df      AIC      BIC    logLik   Test     L.Ratio p-value
    ## model_5r      1  6 53576.48 53622.78 -26782.24                           
    ## model_5br     2  7 53578.48 53632.49 -26782.24 1 vs 2 0.005100146  0.9431

> There is no interaction effect between SD and team average passion.
> There are significant main effects: Higher team passion SD is
> associated with lower workload, and higher team passion average is
> associated with lower workload

Plots for workload:

``` r
eff1 <- effect(c("sd.pas.d"),  model_5)
eff2 <- effect(c("mean.pas.d"),  model_5)

effdata1 <- as.data.frame(eff1)
effdata2 <- as.data.frame(eff2)

plot1 <- ggplot(effdata1, aes(x = sd.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion SD", y="Workload") + theme_classic() + theme(text=element_text(size=12)) 

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(x= "Within-team passion mean", y="Workload") + theme_classic() + theme(text=element_text(size=12)) 

plots <- ggarrange(plot1, plot2, ncol = 2, nrow = 1)
plots <- annotate_figure(plots, top = text_grob("Workload predicted from team passion mean and SD"))

plots
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
## Next day working hours and workload

``` r
# Lead with three measurements (= next day)
d <- d %>%
  mutate(s.wl1.l = lead(s.wl1, n=3L),
         s.wl2.l = lead(s.wl2, n=3L))

# Select only evening measurements (doesn't matter for average team passion/sd but does for stress)

d3 <- d %>%
  filter(type == "e")
```

## Working hours

``` r
model_6 <- lme(fixed = s.wl2.l ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_6)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d3 
    ##        AIC      BIC    logLik
    ##   57581.68 57627.97 -28784.84
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.7994635
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.073041 1.276413
    ## 
    ## Fixed effects:  s.wl2.l ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF  t-value p-value
    ## (Intercept) 10.708798 0.12532813 15736 85.44608  0.0000
    ## sd.pas.d     0.024326 0.03518708 15736  0.69134  0.4894
    ## mean.pas.d  -0.144808 0.01529549 15736 -9.46736  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.559       
    ## mean.pas.d -0.727  0.337
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.6298988 -0.3863784 -0.1305040  0.2089357  5.2142315 
    ## 
    ## Number of Observations: 16567
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_6b <- lme(fixed = s.wl2.l ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit)

summary(model_6b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d3 
    ##        AIC      BIC    logLik
    ##   57588.99 57642.99 -28787.49
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:    0.798448
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:    1.073038  1.27646
    ## 
    ## Fixed effects:  s.wl2.l ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF  t-value p-value
    ## (Intercept)         10.656134 0.19517755 15735 54.59713  0.0000
    ## sd.pas.d             0.067110 0.12641814 15735  0.53086  0.5955
    ## mean.pas.d          -0.133801 0.03484792 15735 -3.83956  0.0001
    ## sd.pas.d:mean.pas.d -0.009287 0.02638082 15735 -0.35205  0.7248
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.836              
    ## mean.pas.d          -0.894  0.904       
    ## sd.pas.d:mean.pas.d  0.767 -0.960 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##        Min         Q1        Med         Q3        Max 
    ## -4.6316577 -0.3863990 -0.1300143  0.2088408  5.2137638 
    ## 
    ## Number of Observations: 16567
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_6r <- lme(fixed = s.wl2.l ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, method = "ML")

model_6br <- lme(fixed = s.wl2.l ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, method = "ML")

anova(model_6r, model_6br)
```

    ##           Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## model_6r      1  6 57566.84 57613.13 -28777.42                         
    ## model_6br     2  7 57568.71 57622.72 -28777.36 1 vs 2 0.1256863  0.7229

> No significant interaction effect. There is only a mean effect for
> average team passion: higher team passion predicts less hours worked
> the next day.

Plot for working hours:

``` r
eff2 <- effect(c("mean.pas.d"),  model_6)

effdata2 <- as.data.frame(eff2)

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(title = "Next day hours worked predicted from team passion average", x= "Within-team passion mean", y="Hours worked next day") + theme_classic() + theme(text=element_text(size=12)) 

plot2
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

## Workload

``` r
model_7 <- lme(fixed = s.wl1.l ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, control = lmeControl(opt = "optim"))

summary(model_7)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d3 
    ##        AIC      BIC    logLik
    ##   56720.96 56767.25 -28354.48
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3826476
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9300844 1.255139
    ## 
    ## Fixed effects:  s.wl1.l ~ sd.pas.d + mean.pas.d 
    ##                 Value  Std.Error    DF   t-value p-value
    ## (Intercept)  3.946171 0.10794686 15736  36.55661  0.0000
    ## sd.pas.d     0.034496 0.03381868 15736   1.02002  0.3077
    ## mean.pas.d  -0.161785 0.01491783 15736 -10.84507  0.0000
    ##  Correlation: 
    ##            (Intr) sd.ps.
    ## sd.pas.d   -0.635       
    ## mean.pas.d -0.826  0.346
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.01770259 -0.48196702 -0.06679893  0.28892453 38.99331450 
    ## 
    ## Number of Observations: 16567
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Interaction
model_7b <- lme(fixed = s.wl1.l ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit)

summary(model_7b)
```

    ## Linear mixed-effects model fit by REML
    ##   Data: d3 
    ##        AIC      BIC    logLik
    ##   56728.41 56782.42 -28357.21
    ## 
    ## Random effects:
    ##  Formula: ~1 | team
    ##         (Intercept)
    ## StdDev:   0.3831133
    ## 
    ##  Formula: ~1 | pid %in% team
    ##         (Intercept) Residual
    ## StdDev:   0.9301283 1.255169
    ## 
    ## Fixed effects:  s.wl1.l ~ sd.pas.d + mean.pas.d + sd.pas.d * mean.pas.d 
    ##                         Value  Std.Error    DF   t-value p-value
    ## (Intercept)          3.972460 0.18189630 15735 21.839145  0.0000
    ## sd.pas.d             0.013183 0.12311481 15735  0.107080  0.9147
    ## mean.pas.d          -0.167274 0.03401246 15735 -4.918009  0.0000
    ## sd.pas.d:mean.pas.d  0.004621 0.02566735 15735  0.180046  0.8571
    ##  Correlation: 
    ##                     (Intr) sd.ps. mn.ps.
    ## sd.pas.d            -0.877              
    ## mean.pas.d          -0.938  0.906       
    ## sd.pas.d:mean.pas.d  0.805 -0.962 -0.899
    ## 
    ## Standardized Within-Group Residuals:
    ##         Min          Q1         Med          Q3         Max 
    ## -4.01419178 -0.48184974 -0.06687005  0.28876436 38.99085100 
    ## 
    ## Number of Observations: 16567
    ## Number of Groups: 
    ##          team pid %in% team 
    ##           155           829

``` r
# Likelihood check
model_7r <- lme(fixed = s.wl1.l ~ sd.pas.d + mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, method = "ML")

model_7br <- lme(fixed = s.wl1.l ~ sd.pas.d + mean.pas.d + sd.pas.d*mean.pas.d,
                   random = ~1 | team/pid, 
                   data = d3, 
                   na.action = na.omit, method = "ML")

anova(model_7r, model_7br)
```

    ##           Model df      AIC      BIC    logLik   Test   L.Ratio p-value
    ## model_7r      1  6 56704.99 56751.28 -28346.49                         
    ## model_7br     2  7 56706.96 56760.97 -28346.48 1 vs 2 0.0317693  0.8585

> No significant interaction effect. There is only a mean effect for
> average team passion: higher team passion predicts less workload the
> next day.

Plot for working hours:

``` r
eff2 <- effect(c("mean.pas.d"),  model_7)

effdata2 <- as.data.frame(eff2)

plot2 <- ggplot(effdata2, aes(x = mean.pas.d, y = fit)) +
  geom_point() +
  geom_line(size = 1.2) + 
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
  labs(title = "Next day workload predicted from team passion average", x= "Within-team passion mean", y="Workload next day") + theme_classic() + theme(text=element_text(size=12)) 

plot2
```

![](Passion-interaction-SD-and-mean_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->
