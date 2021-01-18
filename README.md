# Stratified Analysis of Survival Means and Rates

Zachary McCaw <br>
Updated: 2021-01-18

## Overview

Given stratified time to event data for two arms, this package calculates the mean and ratio of the marginal event rates, restricted mean survival times (RMSTs), and restricted mean times lost (RMTLs). The marginal means/rates are calculated as the stratum-size weighted-average of the per-stratum means/rates. Also see:

* [MargRates](https://github.com/zrmacc/MargRates) for comparing binary event rates.
* [CICs](https://github.com/zrmacc/CICs) for comparing cumulative incidence curves.

## Installation


```r
devtools::install_github(repo = 'zrmacc/StratSurv')
```

## Example

### Data

Below, data are simulated from a two-arm trial with two strata. There are 100 patients in each arm, 50 per stratum. In stratum 1, treatment (`arm == 1`) reduces the event rate by half. In stratum 2, treatment has no effect.


```r
library(StratSurv)
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
##   strata    time status   arm
##    <int>   <dbl>  <dbl> <dbl>
## 1      1  7.82        1     1
## 2      1  2.02        1     1
## 3      1  9.85        0     1
## 4      1 11.6         0     1
## 5      1  0.0162      1     1
## 6      1  8.64        0     1
```

In these data, `arm` is the treatment arm, 0 for reference, 1 for treatment; `time` is the observation time in days; and `status` is the status indicator, 0 for censoring, 1 for recovery; and `stratum` is stratification factor, taking values 0, and 1. For analyzing other data sets, `arm` and `status` should likewise have 0/1 coding. `stratum` may be integer or factor valued.

### Compare RMSTs and RMTLs

To compare RMSTs and RMTLs at $\tau = 18$ months: 


```r
rmst <- StratRMST(
  time = data$time,
  status = data$status,
  arm = data$arm,
  strata = data$strata,
  tau = 18
)
show(rmst)
```

```
## Marginal Statistics:
## # A tibble: 4 x 6
##     arm stat    est    se lower upper
##   <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
## 1     0 RMST  10.9  0.689  9.58 12.3 
## 2     0 RMTL   7.07 0.689  5.72  8.42
## 3     1 RMST  13.2  0.721 11.7  14.6 
## 4     1 RMTL   4.84 0.721  3.43  6.25
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower  upper      p
##   <chr> <chr>     <dbl>  <dbl>  <dbl>  <dbl>
## 1 RMST  A1-A0     2.23   0.273  4.18  0.0255
## 2 RMST  A1/A0     1.2    1.02   1.42  0.0264
## 3 RMTL  A1-A0    -2.23  -4.18  -0.273 0.0255
## 4 RMTL  A1/A0     0.685  0.483  0.971 0.0334
```

The output is on object of class `stratSurv` containing these slots:
* `@Stratified` contains the per arm and stratum summary statistics.
* `@Marginal` contains the per arm RMST and RMTL, marginalized across strata.
* `@Contrasts` contains the difference and ratio of RMSTs and RMTLs.
* `@Weights` contains the stratum sizes and weights. 

By default, the weighting is proportional to stratum size. The weights may also be specified:


```r
rmst <- StratRMST(
  time = data$time,
  status = data$status,
  arm = data$arm,
  strata = data$strata,
  tau = 18,
  weight = c(0.8, 0.2)
)
show(rmst)
```

```
## Marginal Statistics:
## # A tibble: 4 x 6
##     arm stat    est    se lower upper
##   <dbl> <chr> <dbl> <dbl> <dbl> <dbl>
## 1     0 RMST  11    0.844  9.31 12.6 
## 2     0 RMTL   7.03 0.844  5.38  8.69
## 3     1 RMST  13.7  0.828 12    15.3 
## 4     1 RMTL   4.34 0.828  2.71  5.96
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower  upper      p
##   <chr> <chr>     <dbl>  <dbl>  <dbl>  <dbl>
## 1 RMST  A1-A0     2.7    0.381  5.02  0.0225
## 2 RMST  A1/A0     1.25   1.03   1.51  0.0247
## 3 RMTL  A1-A0    -2.7   -5.02  -0.381 0.0225
## 4 RMTL  A1/A0     0.616  0.396  0.959 0.0319
```

### Compare Event Rates

To compare event rates at $\tau = 18$ months: 


```r
rates <- StratRate(
  time = data$time,
  status = data$status,
  arm = data$arm,
  strata = data$strata,
  tau = 18
)
show(rates)
```

```
## Marginal Statistics:
## # A tibble: 2 x 6
##     arm   tau  rate     se lower upper
##   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>
## 1     0    18 0.238 0.0641 0.112 0.363
## 2     1    18 0.506 0.0656 0.377 0.634
## 
## 
## Contrasts:
## # A tibble: 3 x 7
##   strata stat    est     se  lower upper       p
##    <dbl> <chr> <dbl>  <dbl>  <dbl> <dbl>   <dbl>
## 1      1 rd    0.268 0.0917 0.0881 0.448 0.00349
## 2      1 rr    2.13  0.636  1.18   3.82  0.0117 
## 3      1 or    3.28  1.44   1.38   7.77  0.007
```

Here 'rd', 'rr', and 'or' are the rate difference, rate ratio, and rate odds ratio, respectively.
