# Stratified Restricted Mean Survival Time Analysis

Zachary McCaw <br>
Updated: 2021-01-12

## Overview

Given stratified time to event data for two arms, this package calculates the mean and ratio of the marginal restricted mean survival times (RMSTs) and restricted mean times lost (RMTLs). The marginal means are calculated as the stratum-size weighted-average of the per-stratum means. Also see:

* [MargRates](https://github.com/zrmacc/MargRates) for comparing binary event rates.
* [CICs](https://github.com/zrmacc/CICs) for comparing cumulative incidence curves.

## Installation


```r
devtools::install_github(repo = 'zrmacc/StratRMST')
```

## Example

### Data

Below, data are simulated from a two-arm trial with two strata. There are 100 patients in each arm, 50 per stratum. In stratum 1, treatment (`arm == 1`) reduces the event rate by half. In stratum 2, treatment has no effect.


```r
library(StratRMST)
# Arm 1.
data1 <- GenData(
  n = c(50, 50),
  event_rates = c(1/36, 2/36),
  censor_rates = 1/20,
  tau = 36
)
data1$arm <- 1

# Arm 0.
data0 <- GenData(
  n = c(50, 50),
  event_rates = c(2/36, 2/36),
  censor_rates = 1/20,
  tau = 36
)
data0$arm <- 0

# Overall data set.
data <- rbind(data1, data0)
head(data)
```

```
## # A tibble: 6 x 4
## # Groups:   stratum [1]
##   stratum  time status   arm
##     <int> <dbl>  <dbl> <dbl>
## 1       1 26.6       1     1
## 2       1  8.85      0     1
## 3       1  8.28      0     1
## 4       1 20.5       0     1
## 5       1  2.29      0     1
## 6       1  5.88      0     1
```

In these data, `arm` is the treatment arm, 0 for reference, 1 for treatment; `time` is the observation time in days; and `status` is the status indicator, 0 for censoring, 1 for recovery; and `stratum` is stratification factor, taking values 0, and 1. For analyzing other data sets, `arm` and `status` should likewise have 0/1 coding. `stratum` may be integer or factor valued.

### Compare RMSTs and RMTLs

To compare the RMSTs and RMTLs at $\tau = 18$ months: 


```r
strat_analysis <- StratRMST(
  time = data$time,
  status = data$status,
  arm = data$arm,
  strata = data$stratum,
  tau = 18
)
show(strat_analysis)
```

```
## Marginal Statistics:
## # A tibble: 4 x 6
##     arm stat    est    se lower upper
##   <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
## 1     0 RMST  11.4  0.704 10.1  12.8 
## 2     0 RMTL   6.57 0.704  5.19  7.95
## 3     1 RMST  12.8  0.711 11.4  14.2 
## 4     1 RMTL   5.16 0.711  3.77  6.55
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower upper     p
##   <chr> <chr>     <dbl>  <dbl> <dbl> <dbl>
## 1 RMST  A1-A0     1.41  -0.552 3.37  0.159
## 2 RMST  A1/A0     1.12   0.955 1.32  0.16 
## 3 RMTL  A1-A0    -1.41  -3.37  0.552 0.159
## 4 RMTL  A1/A0     0.786  0.558 1.11  0.167
```

The output is on object of class `stratRMST` containing these slots:
* `@Stratified` contains the per arm and stratum summary statistics.
* `@Marginal` contains the per arm RMST and RMTL, marginalized across strata.
* `@Contrasts` contains the difference and ratio of RMSTs and RMTLs.
* `@Weights` contains the stratum sizes and weights. 

By default, the weighting is proportional to stratum size. The weights may also be specified:


```r
strat_analysis <- StratRMST(
  time = data$time,
  status = data$status,
  arm = data$arm,
  strata = data$stratum,
  tau = 18,
  weight = c(0.8, 0.2)
)
show(strat_analysis)
```

```
## Marginal Statistics:
## # A tibble: 4 x 6
##     arm stat    est    se lower upper
##   <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
## 1     0 RMST  11.5  0.849  9.81 13.1 
## 2     0 RMTL   6.53 0.849  4.87  8.19
## 3     1 RMST  13.6  0.8   12    15.2 
## 4     1 RMTL   4.4  0.8    2.84  5.97
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower upper      p
##   <chr> <chr>     <dbl>  <dbl> <dbl>  <dbl>
## 1 RMST  A1-A0     2.13  -0.161 4.41  0.0685
## 2 RMST  A1/A0     1.19   0.985 1.43  0.0723
## 3 RMTL  A1-A0    -2.13  -4.41  0.161 0.0685
## 4 RMTL  A1/A0     0.674  0.435 1.05  0.078
```
