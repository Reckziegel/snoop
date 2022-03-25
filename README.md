
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

`snoop` aims to automate routines for portfolio construction purposes
inside the `tiyverse`.

The current workflow is the following:

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
 X    = stats::rnorm(100, 0, 1),
 Y    = stats::rnorm(100, 0, 2),
 Z    = stats::rnorm(100, 0, 4)
)

# Step 1: Rolling Infraestructure
roll <- construct_rolling_infrastructure(stocks, .initial = 50)
roll
#> # A tibble: 50 x 3
#>    .date      .analysis         .assessment     
#>    <date>     <list>            <list>          
#>  1 2009-02-20 <tibble [50 x 3]> <tibble [1 x 3]>
#>  2 2009-02-21 <tibble [50 x 3]> <tibble [1 x 3]>
#>  3 2009-02-22 <tibble [50 x 3]> <tibble [1 x 3]>
#>  4 2009-02-23 <tibble [50 x 3]> <tibble [1 x 3]>
#>  5 2009-02-24 <tibble [50 x 3]> <tibble [1 x 3]>
#>  6 2009-02-25 <tibble [50 x 3]> <tibble [1 x 3]>
#>  7 2009-02-26 <tibble [50 x 3]> <tibble [1 x 3]>
#>  8 2009-02-27 <tibble [50 x 3]> <tibble [1 x 3]>
#>  9 2009-02-28 <tibble [50 x 3]> <tibble [1 x 3]>
#> 10 2009-03-01 <tibble [50 x 3]> <tibble [1 x 3]>
#> # ... with 40 more rows

# Step 2: Rebalance Infraestructure
rebal <- construct_rebalance_infrastructure(roll, .by = "week")
rebal # information is under the hood
#> # A tibble: 50 x 3
#>    .date      .analysis         .assessment     
#>    <date>     <list>            <list>          
#>  1 2009-02-20 <tibble [50 x 3]> <tibble [1 x 3]>
#>  2 2009-02-21 <tibble [50 x 3]> <tibble [1 x 3]>
#>  3 2009-02-22 <tibble [50 x 3]> <tibble [1 x 3]>
#>  4 2009-02-23 <tibble [50 x 3]> <tibble [1 x 3]>
#>  5 2009-02-24 <tibble [50 x 3]> <tibble [1 x 3]>
#>  6 2009-02-25 <tibble [50 x 3]> <tibble [1 x 3]>
#>  7 2009-02-26 <tibble [50 x 3]> <tibble [1 x 3]>
#>  8 2009-02-27 <tibble [50 x 3]> <tibble [1 x 3]>
#>  9 2009-02-28 <tibble [50 x 3]> <tibble [1 x 3]>
#> 10 2009-03-01 <tibble [50 x 3]> <tibble [1 x 3]>
#> # ... with 40 more rows

# Step 3: Rebalance Portfolio
mu_sigma <- function(.data) {
  # Mean Variance Strategy
  list(mu = colMeans(.data), sigma = stats::cov(.data)) 
}

# Step 4: Compute the main statistics
optimal <- rebalance_portfolio(rebal, mu_sigma, .strategy = "mean_variance")
optimal
#> # A tibble: 50 x 4
#>    .date      .analysis         .assessment      .optimization   
#>    <date>     <list>            <list>           <list>          
#>  1 2009-02-20 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  2 2009-02-21 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  3 2009-02-22 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  4 2009-02-23 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  5 2009-02-24 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  6 2009-02-25 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  7 2009-02-26 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  8 2009-02-27 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#>  9 2009-02-28 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#> 10 2009-03-01 <tibble [50 x 3]> <tibble [1 x 3]> <named list [6]>
#> # ... with 40 more rows

# Step 4: Compute Statistics
extract_statistics(optimal)
#> # A tibble: 50 x 11
#>    .date      .analysis .assessment .optimization .weights  .return .volatility
#>    <date>     <list>    <list>      <list>        <list>      <dbl>       <dbl>
#>  1 2009-02-20 <tibble>  <tibble>    <named list>  <dbl [3]>   0.181       0.965
#>  2 2009-02-21 <tibble>  <tibble>    <named list>  <dbl [3]>   0.477       0.874
#>  3 2009-02-22 <tibble>  <tibble>    <named list>  <dbl [3]>   0.384       0.878
#>  4 2009-02-23 <tibble>  <tibble>    <named list>  <dbl [3]>   0.657       0.873
#>  5 2009-02-24 <tibble>  <tibble>    <named list>  <dbl [3]>  -0.245       0.872
#>  6 2009-02-25 <tibble>  <tibble>    <named list>  <dbl [3]>   0.805       0.869
#>  7 2009-02-26 <tibble>  <tibble>    <named list>  <dbl [3]>   0.929       0.853
#>  8 2009-02-27 <tibble>  <tibble>    <named list>  <dbl [3]>   0.217       0.865
#>  9 2009-02-28 <tibble>  <tibble>    <named list>  <dbl [3]>   0.549       0.865
#> 10 2009-03-01 <tibble>  <tibble>    <named list>  <dbl [3]>   0.482       0.849
#> # ... with 40 more rows, and 4 more variables: .skewness <dbl>,
#> #   .kurtosis <dbl>, .value_at_risk <dbl>, .expected_shortfall <dbl>
```
