# conjest

<!-- badges: start -->
  [![R-CMD-check](https://github.com/kssrr/conjest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kssrr/conjest/actions/workflows/R-CMD-check.yaml)
  [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Provides tools for estimating and visualizing common quantities from conjoint survey experiments, including average marginal component effects (AMCEs) and marginal means (MMs), with support for cluster-robust standard errors and subgroup analyses. Conjoint experiments present respondents with hypothetical profiles that vary simultaneously across multiple attributes, asking them to choose between or rate these profiles. By randomly assigning attribute values across profiles, conjoint designs exploit the logic of randomization to identify the causal effect of each attribute on choices or ratings. This makes them particularly well-suited to studying social judgments and decision-making, where multiple characteristics of a target person or object are likely to jointly influence behavior.

This package implements the methods described in [Hainmueller, Hopkins, and Yamamoto (2014)](https://doi.org/10.1093/pan/mpt024) and [Leeper, Hobolt, and Tilley (2020)](https://doi.org/10.1017/pan.2019.30), allowing you to estimate & visualize:

* Average Marginal Component Effects (AMCEs), via `amce()`.
* Marginal Means, via `mm()`.
* Conditional versions of both, via `conditional_amce()` and `conditional_mm()`.
* Arbitrary models with design-specific adjustments, via `cjlm()`.

Custom S3 methods for summarizing and visualizing results are included for `summary()` and `ggplot2::autoplot()`.

## Installation

The package is not (yet) on CRAN, but you can install it directly from GitHub for now:

```r
devtools::install_github("kssrr/conjest")
```

## Usage

Getting a look at your results is as easy as this:

```r
library(conjest)
data("trust")

amce_res <- amce(trust, selected ~ age + sex, id = ~uuid)

summary(amce_res)

#> Average Marginal Component Effects
#> ============================================================ 
#> 
#> Attribute: age 
#> Reference level: 49 
#> ------------------------------------------------------------ 
#>     Estimate Std. Error t value Pr(>|t|)    
#>  23  -0.0203     0.0084 -2.4298   0.0152   *
#>  67   0.0534     0.0082  6.5395 7.82e-11 ***
#> 
#> Attribute: sex 
#> Reference level: Female 
#> ------------------------------------------------------------ 
#>       Estimate Std. Error  t value Pr(>|t|)    
#>  Male  -0.1041     0.0072 -14.5231 1.65e-45 ***
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Results also work with `ggplot2::autoplot()`, both for `mm()`, as well as for `amce()`:

```r
library(ggplot2)

 trust |> 
  mm(selected ~ sex + age + class, id = ~uuid) |> 
  autoplot()
```
<div align="center">
    <img src="https://github.com/user-attachments/assets/e9cc00ec-2790-42d3-8809-bb38bf15c5da" width="75%">
</div>

```r
autoplot(amce_res)
```
<div align="center">
    <img src="https://github.com/user-attachments/assets/e3bd2861-97ba-42b6-a3da-0dc28b389108" width="75%">
</div>

Everything returns tidy data frames that are easy to work with if you want to make your own visualizations, or present results differently:
```r
amce_res

#> # Average Marginal Component Effects
#> 
#> # A tibble: 5 × 9
#>   attribute level  term   estimate std.error   lower   upper statistic
#>   <chr>     <chr>  <chr>     <dbl>     <dbl>   <dbl>   <dbl>     <dbl>
#> 1 age       49     age49    0        0        0       0          NA
#> 2 age       23     age23   -0.0203   0.00837 -0.0287 -0.0120     -2.43
#> 3 age       67     age67    0.0534   0.00817  0.0452  0.0616      6.54
#> 4 sex       Female sexFe…   0        0        0       0          NA
#> 5 sex       Male   sexMa…  -0.104    0.00717 -0.111  -0.0969    -14.5
#> # ℹ 1 more variable: p.value <dbl>
```
