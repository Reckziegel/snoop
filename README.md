
<!-- README.md is generated from README.Rmd. Please edit that file -->

# snoop

<!-- badges: start -->

[![R-CMD-check](https://github.com/Reckziegel/snoop/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/snoop/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> A package for backtesting and data`snoop`ing.

## Installation

You can install the development version of snoop from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/snoop")
```

## Workflow

1.  Build a rolling `tibble` with `construct_rolling_infrastructure()`
2.  Chose a rebalance periodicity with
    `construct_rebalance_infrastructure()`
3.  Rebalance with `rebalance_portfolio()`
4.  Compute the main statistics with `extract_statistics()`

## Toy Example

``` r
library(snoop)

# Step 0: Get the data
stocks <- tibble::tibble(
 time = as.Date('2009-01-01') + 0:99,
 X    = rnorm(100, 0, 1),
 Y    = rnorm(100, 0, 2),
 Z    = rnorm(100, 0, 4)
)

# Step 1: Rolling Infraestructure
roll <- construct_rolling_infrastructure(stocks, .initial = 50)

# Step 2: Rebalance Infraestructure
rebal <- construct_rebalance_infrastructure(roll)

# Step 3: Rebalance Portfolio
mu_sigma <- function(.data) list(mu = colMeans(.data), sigma = stats::cov(.data)) # Mean Variance Strategy
optimal <- rebalance_portfolio(rebal, mu_sigma, .strategy = "mean_variance")

# Step 4: Compute Statistics
extract_statistics(optimal)
#> # A tibble: 50 x 11
#>    .date      .analysis .assessment .optimization .weights  .return .volatility
#>    <date>     <list>    <list>      <list>        <list>      <dbl>       <dbl>
#>  1 2009-02-20 <tibble>  <tibble>    <named list>  <dbl [3]> -1.86         0.818
#>  2 2009-02-21 <tibble>  <tibble>    <named list>  <dbl [3]> -0.667        0.842
#>  3 2009-02-22 <tibble>  <tibble>    <named list>  <dbl [3]> -0.340        0.840
#>  4 2009-02-23 <tibble>  <tibble>    <named list>  <dbl [3]> -1.13         0.838
#>  5 2009-02-24 <tibble>  <tibble>    <named list>  <dbl [3]>  0.0556       0.845
#>  6 2009-02-25 <tibble>  <tibble>    <named list>  <dbl [3]>  0.876        0.843
#>  7 2009-02-26 <tibble>  <tibble>    <named list>  <dbl [3]> -0.681        0.856
#>  8 2009-02-27 <tibble>  <tibble>    <named list>  <dbl [3]> -1.69         0.861
#>  9 2009-02-28 <tibble>  <tibble>    <named list>  <dbl [3]> -0.504        0.883
#> 10 2009-03-01 <tibble>  <tibble>    <named list>  <dbl [3]> -0.467        0.873
#> # ... with 40 more rows, and 4 more variables: .skewness <dbl>,
#> #   .kurtosis <dbl>, .value_at_risk <dbl>, .expected_shortfall <dbl>
```
