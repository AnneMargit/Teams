CRRC Jaccard Index
================

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.0.5

``` r
library(psych)
library(ggplot2)
library(vegan)
```

    ## Warning: package 'permute' was built under R version 4.0.5

``` r
load("/Volumes/Anne/Harvard/Teams/Teams/d.Rdata")
```

``` r
d <- d[with(d, order(tid, pid, day, type)),]
```

Select in-person interactions

``` r
d_int <- d %>%
  mutate(s.ip_1 = ifelse(s.im_1 == 1, 1, NA),
         s.ip_2 = ifelse(s.im_2 == 1, 1, NA),
         s.ip_3 = ifelse(s.im_3 == 1, 1, NA),
         s.ip_4 = ifelse(s.im_4 == 1, 1, NA),
         s.ip_5 = ifelse(s.im_5 == 1, 1, NA),
         s.ip_6 = ifelse(s.im_6 == 1, 1, NA),
         s.ip_7 = ifelse(s.im_7 == 1, 1, NA))
```

Calculate lags

``` r
d_int <- d_int %>%
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

Make two data sets and recode NAs into 0

``` r
d_int1 <- d_int %>%
  select(s.ip_1:s.ip_7)

d_int1 <- d_int1 %>% replace(is.na(.), 0)

d_int2 <- d_int %>%
  select(s.ip_1.lag:s.ip_7.lag)

d_int2 <- d_int2 %>% replace(is.na(.), 0)
```

Calculate Jaccard index with vegdist from vegan package

``` r
vegdist(rbind(unlist(d_int1, use.names=F), unlist(d_int2, use.names=F)), method="jaccard")
```

    ##           1
    ## 2 0.8771304

= 0.88
