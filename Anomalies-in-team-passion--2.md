Anomalies in team passion \#2
================

``` r
library(knitr)
library(dplyr)
library(anomaly)
library(tidyverse)
library(skimr)
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