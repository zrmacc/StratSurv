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
##   strata   time status   arm
##    <int>  <dbl>  <dbl> <dbl>
## 1      1  1.30       0     1
## 2      1  0.268      0     1
## 3      1 13.1        0     1
## 4      1  2.68       1     1
## 5      1  5.60       0     1
## 6      1  8.21       1     1
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
## 1     0 RMST  13.2  0.672 11.9  14.5 
## 2     0 RMTL   4.82 0.672  3.51  6.14
## 3     1 RMST  12.4  0.698 11    13.7 
## 4     1 RMTL   5.65 0.698  4.28  7.01
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower upper     p
##   <chr> <chr>     <dbl>  <dbl> <dbl> <dbl>
## 1 RMST  A1-A0    -0.822 -2.72   1.08 0.396
## 2 RMST  A1/A0     0.938  0.808  1.09 0.397
## 3 RMTL  A1-A0     0.822 -1.08   2.72 0.396
## 4 RMTL  A1/A0     1.17   0.812  1.69 0.398
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
## 1     0 RMST  12.8  0.837 11.2  14.5 
## 2     0 RMTL   5.15 0.837  3.51  6.79
## 3     1 RMST  13.3  0.741 11.9  14.8 
## 4     1 RMTL   4.66 0.741  3.21  6.11
## 
## 
## Contrasts:
## # A tibble: 4 x 6
##   stat  contrast    est  lower upper     p
##   <chr> <chr>     <dbl>  <dbl> <dbl> <dbl>
## 1 RMST  A1-A0     0.49  -1.7    2.68 0.661
## 2 RMST  A1/A0     1.04   0.878  1.23 0.662
## 3 RMTL  A1-A0    -0.49  -2.68   1.7  0.661
## 4 RMTL  A1/A0     0.905  0.580  1.41 0.66
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
## 1     0    18 0.461 0.0653 0.333 0.589
## 2     1    18 0.438 0.0614 0.317 0.558
## 
## 
## Contrasts:
## # A tibble: 3 x 7
##   strata stat     est     se  lower upper     p
##    <dbl> <chr>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
## 1      1 rd    -0.023 0.0896 -0.199 0.153 0.798
## 2      1 rr     0.95  0.189   0.643 1.4   0.798
## 3      1 or     0.911 0.33    0.448 1.85  0.798
```
