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

Make datasets for different team sizes (4, 5, 6, or 7)

``` r
d_int_4 <- d_int %>% filter(team_size == 4)

d_int_5 <- d_int %>% filter(team_size == 5)

d_int_6 <- d_int %>% filter(team_size == 6)

d_int_7 <- d_int %>% filter(team_size == 7)
```

Make two data sets for each (one at time t and one at time t+1)

``` r
# 4 member teams
d_int_4a <- d_int_4 %>% select(s.ip_1:s.ip_4)

d_int_4a <- d_int_4a %>% replace(is.na(.), 0)

d_int_4b <- d_int_4 %>% select(s.ip_1.lag:s.ip_4.lag)

d_int_4b <- d_int_4b %>% replace(is.na(.), 0)

# 5 member teams
d_int_5a <- d_int_5 %>% select(s.ip_1:s.ip_5)

d_int_5a <- d_int_5a %>% replace(is.na(.), 0)

d_int_5b <- d_int_5 %>% select(s.ip_1.lag:s.ip_5.lag)

d_int_5b <- d_int_5b %>% replace(is.na(.), 0)

# 6 member teams
d_int_6a <- d_int_6 %>% select(s.ip_1:s.ip_6)

d_int_6a <- d_int_6a %>% replace(is.na(.), 0)

d_int_6b <- d_int_6 %>% select(s.ip_1.lag:s.ip_6.lag)

d_int_6b <- d_int_6b %>% replace(is.na(.), 0)

# 7 member teams
d_int_7a <- d_int_7 %>% select(s.ip_1:s.ip_7)

d_int_7a <- d_int_7a %>% replace(is.na(.), 0)

d_int_7b <- d_int_7 %>% select(s.ip_1.lag:s.ip_7.lag)

d_int_7b <- d_int_7b %>% replace(is.na(.), 0)
```

Calculate Jaccard index with vegdist from vegan package

``` r
# 4 member teams
vegdist(rbind(unlist(d_int_4a, use.names=F), unlist(d_int_4b, use.names=F)), method="jaccard")
```

    ##           1
    ## 2 0.9101782

``` r
# 5 member teams
vegdist(rbind(unlist(d_int_5a, use.names=F), unlist(d_int_5b, use.names=F)), method="jaccard")
```

    ##           1
    ## 2 0.8737681

``` r
# 6 member teams
vegdist(rbind(unlist(d_int_6a, use.names=F), unlist(d_int_6b, use.names=F)), method="jaccard")
```

    ##           1
    ## 2 0.8765864

``` r
# 7 member teams
vegdist(rbind(unlist(d_int_7a, use.names=F), unlist(d_int_7b, use.names=F)), method="jaccard")
```

    ##           1
    ## 2 0.8710136

4 member teams: 0.91

5 member teams: 0.87

6 member teams: 0.88

7 member teams: 0.87
