Anomalies in team passion \#2
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
load(file="d.Rdata")
```

Team average passion

``` r
d <- d %>%
  group_by(pid) %>%
  mutate(m = 1:n()) %>%
  ungroup()

# There are 10 measurements with missings on passion, I delete these 
d <- d %>%
  filter(!is.na(s.pas))

# Team passion average and SD per measurement
d <- d %>%
  group_by(m, team) %>%
  mutate(sd.pas = sd(s.pas, na.rm=TRUE),
         mean.pas = mean(s.pas, na.rm=TRUE)) %>%
  ungroup()
```

``` r
d_t <-d %>%
  group_by(team, m) %>%
  slice(1) %>%
  ungroup()

df <- d_t[order(d_t$m),] 
df <- df %>%
  select(team, m, mean.pas)
```

Remove teams with no variance (N= 22)

``` r
df %>%
  group_by(team) %>%
  dplyr::summarise(mad = mad(mean.pas)) %>%
  arrange(mad)
```

    ## # A tibble: 155 × 2
    ##     team   mad
    ##    <dbl> <dbl>
    ##  1  1024     0
    ##  2  1025     0
    ##  3  1027     0
    ##  4  1028     0
    ##  5  1030     0
    ##  6  1031     0
    ##  7  1032     0
    ##  8  1033     0
    ##  9  1034     0
    ## 10  1036     0
    ## # … with 145 more rows

``` r
# I remove those teams
df <- df %>%
  group_by(team) %>%
  dplyr::mutate(mad = mad(mean.pas))

df <- df %>%
  filter(mad > 0)

df <- df %>%
  select(-mad)

n_distinct(df$team)
```

    ## [1] 128

``` r
# This leaves 128 teams
```

Function to get anomalies (min length = 3, max length = 9)

``` r
get.an <- function(x) {
  # FOR TESTING:
  # x = df
  # i = 1003
  
  x$start <- NA
  x$end <- NA
  x$section <- NA
  x$mean.change <- NA
  x$test.statistic <- NA
  
  for (i in unique(x$team)) {
    # reset for error handling
    res.capa <- NA
    
    tryCatch({
      res.capa <-
        capa.uv(
          x$mean.pas[x$team == i],
          min_seg_len = 3,
          max_seg_len = 9,
          type = "robustmean"
        )
      res <- collective_anomalies(res.capa)
      cat("Team", i, "has", nrow(res), "anomalous segments.\n")
    }, error = function(e) {
      message("ERROR: Team ", i, ": ", conditionMessage(e))
    })
    
    if (is.na(res.capa)) { # If error detected
      x[x$team == i,] <-
        x %>%
        filter(team == i) %>%
        mutate(start = NaN, # set to NaN to distinguish it from unprocessed or working values
               end = NaN, 
               section = NaN,
               mean.change = NaN)
      
    } else if (nrow(res) > 0) { # If anomaly detected
      
      anomalySection <- mapply(seq, res$start, res$end, SIMPLIFY = FALSE) %>% unlist
      
      x[x$team == i,] <-
        x %>%
        # so that the join doesn't create duplicate columns:
        select(-c("mean.change", "test.statistic")) %>% 
        filter(team == i) %>%
        mutate(start = ifelse(m %in% res$start, 1, 0),
               end = ifelse(m %in% res$end, 1, 0),
               section = ifelse(m %in% anomalySection, cumsum(start), 0)) %>% # cumulative number to indicate number of anomaly periods 
        left_join(res %>% select(-end), by = c('m' = 'start')) #%>%  
        # optionally, uncomment the next line to make the NAs into zeros
        #mutate(across(c("mean.change", "test.statistic"), ~replace_na(.x, 0)))
      
    } else { # If no anomaly detected
      x[x$team == i,] <-
        x %>%
        filter(team == i) %>%
        mutate(start = NA, 
               end = NA, 
               section = NA,
               mean.change = NA, 
               test.statistic = NA)
    }
  }
  return(x)
}

anomalies <- get.an(df) %>% arrange(team)
```

Calculate length of anomalies

``` r
anomalies <- anomalies %>%
  group_by(team, section) %>%
  mutate(length = n())

anomalies <- anomalies %>%
  mutate(length = ifelse(section > 0, length, NA))
```

Calculate the mean passion score during each anomaly period:

``` r
anomalies <- anomalies %>%
  group_by(team) %>%
  mutate(team.mean = mean(mean.pas), # average passion for team across entire period
         anomalies.count = sum(start), # total number of anomalies
         section.mean = ifelse(section == 1, mean(mean.pas[section == 1]), # average passion during anomaly period 1 
                               ifelse(section == 2, mean(mean.pas[section == 2]),
                                      ifelse(section ==3, mean(mean.pas[section == 3]),
                                             ifelse(section ==4, mean(mean.pas[section == 4]),
                                                    ifelse(section ==5, mean(mean.pas[section == 5]),
                                                           ifelse(section == 6, mean(mean.pas[section == 6]), NA))))))) %>%
  ungroup()
```

How many anomalies are there across the teams?

``` r
anomalies2 <- anomalies %>% group_by(team) %>% slice(1)

anomalies2$anomalies.count[is.na(anomalies2$anomalies.count)] <- 0

table(anomalies2$anomalies.count)
```

    ## 
    ##  0  1  2  3  4  5  6 
    ## 25 39 25 26 10  2  1

``` r
skim(anomalies2$anomalies.count)
```

|                                                  |                            |
|:-------------------------------------------------|:---------------------------|
| Name                                             | anomalies2$anomalies.coun… |
| Number of rows                                   | 128                        |
| Number of columns                                | 1                          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                            |
| Column type frequency:                           |                            |
| numeric                                          | 1                          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                            |
| Group variables                                  | None                       |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| data          |         0 |             1 | 1.74 | 1.35 |   0 |   1 | 1.5 |   3 |    6 | ▇▃▃▁▁ |

> On average, teams have 1.74 anomalies

Is the anomaly period a boom (higher than average passion across entire
study period) or a bust (lower than average)?

``` r
# I indicate booms with 2 and busts with 1
anomalies <- anomalies %>%
  mutate(section.bb = ifelse(section.mean > team.mean, 2, # boom = 2
                             ifelse(section.mean < team.mean, 1, 0))) # bust = 1, other = 0

anomalies2 <- anomalies %>%
  mutate(section.bb = ifelse(start == 0, NA, section.bb)) 

anomalies2 <- anomalies2 %>%
  group_by(team)%>%
  add_tally(section.bb[section.bb == 2]) 

anomalies2 <- anomalies2 %>%
  dplyr::rename(boom.count = n)

anomalies2 <- anomalies2 %>%
  group_by(team) %>%
  mutate(boom.count = boom.count / 2)

anomalies2 <- anomalies2 %>%
  group_by(team)%>%
  add_tally(section.bb[section.bb == 1]) 

anomalies2 <- anomalies2 %>%
  dplyr::rename(bust.count = n)
```

How many booms are there?

``` r
anomalies.b <- anomalies2 %>%
  group_by(team)%>%
  slice(1)%>%
  ungroup()

# Average length of booms
anomalies.b %>%
  filter(section.bb == 2) %>%
  summarise(mean.length = mean(length, na.rm=T),
            sd.length = sd(length),
            min.length = min(length),
            max.length = max(length),
            median.length = median(length))
```

    ## # A tibble: 1 × 5
    ##   mean.length sd.length min.length max.length median.length
    ##         <dbl>     <dbl>      <int>      <int>         <dbl>
    ## 1        4.33      2.16          3          8             3

``` r
# Mean length = 4.33, median = 3

table(anomalies.b$boom.count)
```

    ## 
    ##  0  1  2  3 
    ## 79 31 16  2

``` r
skim(anomalies.b$boom.count)
```

|                                                  |                        |
|:-------------------------------------------------|:-----------------------|
| Name                                             | anomalies.b$boom.count |
| Number of rows                                   | 128                    |
| Number of columns                                | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                        |
| Column type frequency:                           |                        |
| numeric                                          | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                        |
| Group variables                                  | None                   |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| data          |         0 |             1 | 0.54 | 0.77 |   0 |   0 |   0 |   1 |    3 | ▇▃▁▂▁ |

How many busts are there?

``` r
table(anomalies.b$bust.count)
```

    ## 
    ##  0  1  2  3  4  5 
    ## 37 47 29 12  2  1

``` r
# Average length of busts
anomalies.b %>%
  filter(section.bb == 1) %>%
  summarise(mean.length = mean(length, na.rm=T),
            sd.length = sd(length),
            min.length = min(length),
            max.length = max(length),
            median.length = median(length))
```

    ## # A tibble: 1 × 5
    ##   mean.length sd.length min.length max.length median.length
    ##         <dbl>     <dbl>      <int>      <int>         <int>
    ## 1        3.89      2.03          3          9             3

``` r
# Mean = 3.88, median = 3

skim(anomalies.b$bust.count)
```

|                                                  |                        |
|:-------------------------------------------------|:-----------------------|
| Name                                             | anomalies.b$bust.count |
| Number of rows                                   | 128                    |
| Number of columns                                | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                        |
| Column type frequency:                           |                        |
| numeric                                          | 1                      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                        |
| Group variables                                  | None                   |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |  p0 | p25 | p50 | p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|----:|----:|----:|----:|-----:|:------|
| data          |         0 |             1 |  1.2 | 1.06 |   0 |   0 |   1 |   2 |    5 | ▇▃▁▁▁ |

> On average, teams have 0.54 booms (anomaly period with higher passion
> than the rest of the study) and 1.20 busts (anomaly period with lower
> passion)

Make two variables to indicate boom and bust instead of one

``` r
anomalies.data <- anomalies %>%
  mutate(boom = ifelse(section.bb == 2, 1, 0),
         bust = ifelse(section.bb == 1, 1, 0))
```

Also make two variables to indicate the length of booms/busts

``` r
anomalies.data <- anomalies.data %>%
  mutate(length.boom = ifelse(section.bb == 2, length, NA),
         length.bust = ifelse(section.bb == 1, length, NA))
```

And make dummy variables of these. Booms/busts can be between 3 and 9
measurements long. I recode length of 6 and higher as long (so 6,7,8 or
9) with 2, and below 6 as short (so 3, 4, or 5) with 1, periods with no
booms/busts as 0

``` r
anomalies.data <- anomalies.data %>%
  mutate(length.boom.d = ifelse(length.boom < 6, 1,
                                ifelse(length.boom > 5, 2, 0)),
        length.bust.d = ifelse(length.bust < 6, 1,
                               ifelse(length.bust > 5, 2, 0)))
```

Indicate with the columns after.boom.phase/after.bust.phase if the
measurement is after a bust/boom

``` r
anomalies.data <- anomalies.data %>%
  mutate(after.boom = ifelse(lag(boom) == 1 & lag(end)== 1, 1, 0),
         after.boom2 = ifelse(lag(after.boom) == 1, 1, 0),
         after.boom3 = ifelse(lag(after.boom2) == 1, 1, 0),
         after.boom4 = ifelse(lag(after.boom3) == 1, 1, 0),
         after.boom.phase = after.boom + after.boom2 + after.boom3 + after.boom4) %>%
  select(-c(after.boom, after.boom2, after.boom3, after.boom4))

anomalies.data <- anomalies.data %>%
  mutate(after.bust = ifelse(lag(bust) == 1 & lag(end)== 1, 1, 0),
         after.bust2 = ifelse(lag(after.bust) == 1, 1, 0),
         after.bust3 = ifelse(lag(after.bust2) == 1, 1, 0),
         after.bust4 = ifelse(lag(after.bust3) == 1, 1, 0),
         after.bust.phase = after.bust + after.bust2 + after.bust3 + after.bust4) %>%
  select(-c(after.bust, after.bust2, after.bust3, after.bust4))

anomalies.data <- anomalies.data %>% 
  mutate(after.boom.phase2 = ifelse(after.boom.phase == 1, 1, 
                                    ifelse(boom == 1, 0, NA))) %>%
  select(-c(after.boom.phase))

anomalies.data <- anomalies.data %>% 
  mutate(after.bust.phase2 = ifelse(after.bust.phase == 1, 1, 
                                    ifelse(bust == 1, 0, NA))) %>%
  select(-c(after.bust.phase))
    
anomalies.data <- anomalies.data %>% 
  dplyr::rename(after.boom.phase = after.boom.phase2,
                after.bust.phase = after.bust.phase2)
```

Create a dummy to indicate if the measurement is after a long or short
boom/bust This is the column after.boom.length.d or after.bust.length.d
(2 = after long one, 1 is after short one, 0 = during boom/bust)

``` r
anomalies.data <- anomalies.data %>%
  mutate(after.boom.d = ifelse(lag(length.boom.d) == 2 & lag(end) == 1, 2,
                               ifelse(lag(length.boom.d) == 1 & lag(end) == 1, 1, 0)),
         after.boom.d2 = ifelse(lag(after.boom.d) == 2, 2,
                                ifelse(lag(after.boom.d) == 1, 1, 0)),
         after.boom.d3 = ifelse(lag(after.boom.d2) == 2, 2,
                                ifelse(lag(after.boom.d2) == 1, 1, 0)),
         after.boom.d4 = ifelse(lag(after.boom.d3) == 2, 2,
                                ifelse(lag(after.boom.d3) == 1, 1, 0)),
         after.boom.length.d = after.boom.d + after.boom.d2 + after.boom.d3 + after.boom.d4) %>%
  select(-c(after.boom.d, after.boom.d2, after.boom.d3, after.boom.d4))

anomalies.data <- anomalies.data %>%
  mutate(after.boom.length.d = ifelse(is.na(after.boom.phase), NA, after.boom.length.d))

anomalies.data <- anomalies.data %>%
  mutate(after.bust.d = ifelse(lag(length.bust.d) == 2 & lag(end) == 1, 2,
                               ifelse(lag(length.bust.d) == 1 & lag(end) == 1, 1, 0)),
         after.bust.d2 = ifelse(lag(after.bust.d) == 2, 2,
                                ifelse(lag(after.bust.d) == 1, 1, 0)),
         after.bust.d3 = ifelse(lag(after.bust.d2) == 2, 2,
                                ifelse(lag(after.bust.d2) == 1, 1, 0)),
         after.bust.d4 = ifelse(lag(after.bust.d3) == 2, 2,
                                ifelse(lag(after.bust.d3) == 1, 1, 0)),
         after.bust.length.d = after.bust.d + after.bust.d2 + after.bust.d3 + after.bust.d4) %>%
  select(-c(after.bust.d, after.bust.d2, after.bust.d3, after.bust.d4))

anomalies.data <- anomalies.data %>%
  mutate(after.bust.length.d = ifelse(is.na(after.bust.phase), NA, after.bust.length.d))
```

Save

``` r
save(anomalies.data, file = "anomalies.data.Rdata")
```

# Join with individual-level data

``` r
d.an <- d %>%
  left_join(anomalies.data, by = c("team", "m", "mean.pas"))

# This makes that only the first measurement occassion of the bust/boom is coded  
d.an.first <- d.an %>%
  mutate(boom = ifelse(section.bb == 2 & start == 1, 1, 0),
         bust = ifelse(section.bb == 1 & start == 1, 1, 0))

# Replace na with zero
d.an.first$boom[is.na(d.an.first$boom)] <- 0
d.an.first$bust[is.na(d.an.first$bust)] <- 0
d.an.first$length.boom[is.na(d.an.first$length.boom)] <- 0

# This makes that only the last measurement occassion of the bust/boom is coded  
d.an.last <- d.an %>%
  mutate(boom = ifelse(section.bb == 2 & end == 1, 1, 0),
         bust = ifelse(section.bb == 1 & end == 1, 1, 0))

# Replace na with zero
d.an.last$boom[is.na(d.an.last$boom)] <- 0
d.an.last$bust[is.na(d.an.last$bust)] <- 0
d.an.last$length.boom[is.na(d.an.last$length.boom)] <- 0
d.an.last$length.bust[is.na(d.an.last$length.bust)] <- 0

d.an.an <- d.an

# Not replacing NAs with zeroes
save(d.an.an, file = "d.an.an.Rdata")

# Replace na with zero
d.an$boom[is.na(d.an$boom)] <- 0
d.an$bust[is.na(d.an$bust)] <- 0
d.an$length.boom[is.na(d.an$length.boom)] <- 0
d.an$length.bust[is.na(d.an$length.bust)] <- 0
```

Save

``` r
save(d.an, file="d.an.Rdata")
save(d.an.first, file = "d.an.first.Rdata")
save(d.an.last, file = "d.an.last.Rdata")
```
