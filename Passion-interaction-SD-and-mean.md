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
