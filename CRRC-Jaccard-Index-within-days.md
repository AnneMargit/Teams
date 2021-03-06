CRRC Jaccard Index within days
================

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

``` r
library(psych)
library(vegan)
```

    ## Warning: package 'permute' was built under R version 4.0.5

``` r
library(knitr)
```

``` r
load("/Volumes/Anne/Harvard/Teams/Teams/d.Rdata")
```

``` r
d <- d[with(d, order(tid, pid, day, type)),]
```

## Within-day, all forms of interaction

Calculate lags

``` r
d <- d %>%
  group_by(pid, day) %>%
  mutate(s.ip_1.lag = lag(s.ip_1, n=1L, default=NA),
         s.ip_2.lag = lag(s.ip_2, n=1L, default=NA),
         s.ip_3.lag = lag(s.ip_3, n=1L, default=NA),
         s.ip_4.lag = lag(s.ip_4, n=1L, default=NA),
         s.ip_5.lag = lag(s.ip_5, n=1L, default=NA),
         s.ip_6.lag = lag(s.ip_6, n=1L, default=NA),
         s.ip_7.lag = lag(s.ip_7, n=1L, default=NA)) %>%
  ungroup()
```

Make datasets for different team sizes (4, 5, 6, or 7)

``` r
d_4 <- d %>% filter(team_size == 4)

d_5 <- d %>% filter(team_size == 5)

d_6 <- d %>% filter(team_size == 6)

d_7 <- d %>% filter(team_size == 7)
```

Make two data sets for each (one at time t and one at time t+1)

``` r
# All teams
d_a <- d %>% select(s.ip_1:s.ip_7)

d_a <- d_a %>% replace(is.na(.), 0)

d_b <- d %>% select(s.ip_1.lag:s.ip_7.lag)

d_b <- d_b %>% replace(is.na(.), 0)

# 4 member teams
d_4a <- d_4 %>% select(s.ip_1:s.ip_4)

d_4a <- d_4a %>% replace(is.na(.), 0)

d_4b <- d_4 %>% select(s.ip_1.lag:s.ip_4.lag)

d_4b <- d_4b %>% replace(is.na(.), 0)

# 5 member teams
d_5a <- d_5 %>% select(s.ip_1:s.ip_5)

d_5a <- d_5a %>% replace(is.na(.), 0)

d_5b <- d_5 %>% select(s.ip_1.lag:s.ip_5.lag)

d_5b <- d_5b %>% replace(is.na(.), 0)

# 6 member teams
d_6a <- d_6 %>% select(s.ip_1:s.ip_6)

d_6a <- d_6a %>% replace(is.na(.), 0)

d_6b <- d_6 %>% select(s.ip_1.lag:s.ip_6.lag)

d_6b <- d_6b %>% replace(is.na(.), 0)

# 7 member teams
d_7a <- d_7 %>% select(s.ip_1:s.ip_7)

d_7a <- d_7a %>% replace(is.na(.), 0)

d_7b <- d_7 %>% select(s.ip_1.lag:s.ip_7.lag)

d_7b <- d_7b %>% replace(is.na(.), 0)
```

Calculate Jaccard index with vegdist from vegan package

``` r
# All teams
vegdist(rbind(unlist(d_a, use.names=F), unlist(d_b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.6965589

``` r
# 4 member teams
vegdist(rbind(unlist(d_4a, use.names=F), unlist(d_4b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.7033048

``` r
# 5 member teams
vegdist(rbind(unlist(d_5a, use.names=F), unlist(d_5b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.6985525

``` r
# 6 member teams
vegdist(rbind(unlist(d_6a, use.names=F), unlist(d_6b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.6981276

``` r
# 7 member teams
vegdist(rbind(unlist(d_7a, use.names=F), unlist(d_7b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.6893025

-   All: 0.69
-   4 member teams: 0.70
-   5 member teams: 0.70
-   6 member teams: 0.70
-   7 member teams: 0.69

## Within-day, in-person interactions

Select in-person interactions

``` r
d_inp <- d %>%
  mutate(s.ip_1 = ifelse(s.im_1 == 1, 1, NA),
         s.ip_2 = ifelse(s.im_2 == 1, 1, NA),
         s.ip_3 = ifelse(s.im_3 == 1, 1, NA),
         s.ip_4 = ifelse(s.im_4 == 1, 1, NA),
         s.ip_5 = ifelse(s.im_5 == 1, 1, NA),
         s.ip_6 = ifelse(s.im_6 == 1, 1, NA),
         s.ip_7 = ifelse(s.im_7 == 1, 1, NA))
```

Make datasets for different team sizes (4, 5, 6, or 7)

``` r
d_inp_4 <- d_inp %>% filter(team_size == 4)

d_inp_5 <- d_inp %>% filter(team_size == 5)

d_inp_6 <- d_inp %>% filter(team_size == 6)

d_inp_7 <- d_inp %>% filter(team_size == 7)
```

Make two data sets for each (one at time t and one at time t+1)

``` r
# All teams
d_inp_a <- d_inp %>% select(s.ip_1:s.ip_7)

d_inp_a <- d_inp_a %>% replace(is.na(.), 0)

d_inp_b <- d_inp %>% select(s.ip_1.lag:s.ip_7.lag)

d_inp_b <- d_inp_b %>% replace(is.na(.), 0)

# 4 member teams
d_inp_4a <- d_inp_4 %>% select(s.ip_1:s.ip_4)

d_inp_4a <- d_inp_4a %>% replace(is.na(.), 0)

d_inp_4b <- d_inp_4 %>% select(s.ip_1.lag:s.ip_4.lag)

d_inp_4b <- d_inp_4b %>% replace(is.na(.), 0)

# 5 member teams
d_inp_5a <- d_inp_5 %>% select(s.ip_1:s.ip_5)

d_inp_5a <- d_inp_5a %>% replace(is.na(.), 0)

d_inp_5b <- d_inp_5 %>% select(s.ip_1.lag:s.ip_5.lag)

d_inp_5b <- d_inp_5b %>% replace(is.na(.), 0)

# 6 member teams
d_inp_6a <- d_inp_6 %>% select(s.ip_1:s.ip_6)

d_inp_6a <- d_inp_6a %>% replace(is.na(.), 0)

d_inp_6b <- d_inp_6 %>% select(s.ip_1.lag:s.ip_6.lag)

d_inp_6b <- d_inp_6b %>% replace(is.na(.), 0)

# 7 member teams
d_inp_7a <- d_inp_7 %>% select(s.ip_1:s.ip_7)

d_inp_7a <- d_inp_7a %>% replace(is.na(.), 0)

d_inp_7b <- d_inp_7 %>% select(s.ip_1.lag:s.ip_7.lag)

d_inp_7b <- d_inp_7b %>% replace(is.na(.), 0)
```

Calculate Jaccard index

``` r
# All
vegdist(rbind(unlist(d_inp_a, use.names=F), unlist(d_inp_b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.8475609

``` r
# 4 member teams
vegdist(rbind(unlist(d_inp_4a, use.names=F), unlist(d_inp_4b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.8610488

``` r
# 5 member teams
vegdist(rbind(unlist(d_inp_5a, use.names=F), unlist(d_inp_5b, use.names=F)), method="jaccard", binary=T)
```

    ##          1
    ## 2 0.845979

``` r
# 6 member teams
vegdist(rbind(unlist(d_inp_6a, use.names=F), unlist(d_inp_6b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.8464185

``` r
# 7 member teams
vegdist(rbind(unlist(d_inp_7a, use.names=F), unlist(d_inp_7b, use.names=F)), method="jaccard", binary=T)
```

    ##           1
    ## 2 0.8463957

-   All: 0.85
-   4 members: 0.86
-   5 members: 0.85
-   6 members: 0.85
-   7 members: 0.85
