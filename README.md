
# Stratified Restricted Mean Survival Time Analysis

Zachary McCaw <br>
Updated: 2020-11-17

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

Synthetic example data in the format expected by this package may be loaded via:


```r
library(StratRMST)
data(strat_data)
head(strat_data)
```

```
##   arm   time status stratum
## 1   0 0.6415      1       0
## 2   0 0.6415      1       0
## 3   0 0.6415      1       0
## 4   0 0.6415      1       0
## 5   0 1.0000      1       0
## 6   0 1.0000      1       0
```

In these data, `arm` is the treatment arm, 0 for reference, 1 for treatment; `time` is the observation time in days; and `status` is the status indicator, 0 for censoring, 1 for recovery; and `stratum` is stratification factor, taking values 0, 1, and 2. For analyzing other data sets, `arm` and `status` should likewise have 0/1 coding. `stratum` may be integer or factor valued.

### Compare RMSTs and RMTLs

To compare the RMSTs and RMTLs at $\tau = 18$ months: 


```r
strat_analysis <- StratRMST(
  time = strat_data$time,
  status = strat_data$status,
  arm = strat_data$arm,
  strata = strat_data$stratum,
  tau = 18
)
show(strat_analysis)
```

```
## Marginal Statistics:
##   Arm Tau Stat   Est    SE     L     U
## 1   0  18 RMST 11.50 0.362 10.80 12.20
## 2   1  18 RMST 14.00 0.353 13.30 14.70
## 3   0  18 RMTL  6.52 0.362  5.81  7.23
## 4   1  18 RMTL  3.97 0.353  3.28  4.66
## 
## 
## Contrasts:
##   Stat Contrast    Est      L      U        P
## 1 RMST    A1-A0  2.550  1.560  3.540 4.70e-07
## 2 RMST    A1/A0  1.220  1.130  1.320 6.82e-07
## 3 RMTL    A1-A0 -2.550 -3.540 -1.560 4.70e-07
## 4 RMTL    A1/A0  0.609  0.496  0.748 2.24e-06
```

The output is on object of class `stratRMST` containing these slots:
* `@Stratified` contains the per arm and stratum summary statistics.
* `@Marginal` contains the per arm RMST and RMTL, marginalized across strata.
* `@Contrasts` contains the difference and ratio of RMSTs and RMTLs.
* `@Weights` contains the stratum sizes and weights. 
