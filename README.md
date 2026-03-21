# conjest

Basic estimands for conjoint experiments. Conjoint experiments present respondents with hypothetical profiles that vary simultaneously across multiple attributes, asking them to choose between or rate these profiles. By randomly assigning attribute values across profiles, conjoint designs exploit the logic of randomization to identify the causal effect of each attribute on choices or ratings. This makes them particularly well-suited to studying social judgments and decision-making, where multiple characteristics of a target person or object are likely to jointly influence behavior.

This package allows you to estimate & visualize:

* Average Marginal Component Effects (AMCEs), via `amce()`.
* Marginal Means, via `marginal_means()`.
* *Planned:* Average Component Interaction Effects (ACIEs).
* *Planned:* Conditional Marginal Means, for subgroup analysis.

## Installation

The package is not (yet) on CRAN, but you can install it directly from GitHub:

```r
devtools::install_github("kssrr/conjest")
```

## Usage

Getting a look at your results is as easy as this:

```r
library(conjest)

amce_res <- 
  immigration |> 
  amce(ChosenImmigrant ~ Education + Gender + CountryOfOrigin, id = ~CaseID) |> 

summary(amce_res)

#> Average Marginal Component Effects
#> ============================================================ 
#> 
#> Attribute: Education 
#> Reference level: No Formal 
#> ------------------------------------------------------------ 
#>                   Estimate Std. Error t value Pr(>|t|)     
#>         4th Grade   0.0327     0.0155   2.109 3.50e-02  *  
#>         8th Grade   0.0549     0.0156   3.529 4.19e-04  ***
#>       High School   0.1180     0.0157   7.521 5.77e-14  ***
#>  Two-Year College   0.1761     0.0158  11.152 9.22e-29  ***
#>    College Degree   0.1994     0.0160  12.446 2.27e-35  ***
#>   Graduate Degree   0.1918     0.0159  12.037 3.32e-33  ***
#> 
#> Attribute: Gender 
#> Reference level: Female 
#> ------------------------------------------------------------ 
#>       Estimate Std. Error t value Pr(>|t|)     
#>  Male  -0.0241     0.0085  -2.823 4.76e-03  ** 
#> 
#> Attribute: CountryOfOrigin 
#> Reference level: India 
#> ------------------------------------------------------------ 
#>              Estimate Std. Error t value Pr(>|t|)     
#>      Germany   0.0407     0.0182   2.234 2.55e-02  *  
#>       France   0.0149     0.0186   0.798 4.25e-01     
#>       Mexico   0.0107     0.0187   0.570 5.69e-01     
#>  Philippines   0.0298     0.0183   1.626 1.04e-01     
#>       Poland   0.0250     0.0186   1.342 1.80e-01     
#>        China  -0.0021     0.0185  -0.116 9.08e-01     
#>        Sudan  -0.0208     0.0190  -1.093 2.74e-01     
#>      Somalia  -0.0285     0.0188  -1.519 1.29e-01     
#>         Iraq  -0.0932     0.0195  -4.779 1.78e-06  ***
#> 
#> 
#> Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Results also work with `ggplot2::autoplot()`, both for `marginal_means()`, as well as for `amce()`:

```r
library(ggplot2)

immigration |> 
  marginal_means(ChosenImmigrant ~ Education + Gender + CountryOfOrigin, id = ~CaseID) |> 
  autoplot()
```
![](https://github.com/user-attachments/assets/e133896f-f36d-48a4-ad5e-c5a13e402421)

```r
autoplot(amce_res)
```
![](https://github.com/user-attachments/assets/b68d8756-8411-4532-b3f7-847450d026d2)

Everything returns tidy data frames that are easy to work with if you want to make your own visualizations, or present results differently:
```r
amce_res

#> # Average Marginal Component Effects
#>
#> # A tibble: 19 × 9
#>    term       estimate std.error    lower    upper statistic   p.value
#>    <chr>         <dbl>     <dbl>    <dbl>    <dbl>     <dbl>     <dbl>
#>  1 Education…  0         0        0        0           0     NA       
#>  2 Education…  0.0327    0.0155   0.0172   0.0482      2.11   3.50e- 2
#>  3 Education…  0.0549    0.0156   0.0394   0.0705      3.53   4.19e- 4
#>  4 Education…  0.118     0.0157   0.102    0.134       7.52   5.77e-14
#>  5 Education…  0.176     0.0158   0.160    0.192      11.2    9.22e-29
#>  6 Education…  0.199     0.0160   0.183    0.215      12.4    2.27e-35
#>  7 Education…  0.192     0.0159   0.176    0.208      12.0    3.32e-33
#>  8 GenderFem…  0         0        0        0           0     NA       
#>  9 GenderMale -0.0241    0.00853 -0.0326  -0.0156     -2.82   4.76e- 3
#> 10 CountryOf…  0         0        0        0           0     NA       
#> 11 CountryOf…  0.0407    0.0182   0.0225   0.0590      2.23   2.55e- 2
#> 12 CountryOf…  0.0149    0.0186  -0.00375  0.0335      0.798  4.25e- 1
#> 13 CountryOf…  0.0107    0.0187  -0.00807  0.0294      0.570  5.69e- 1
#> 14 CountryOf…  0.0298    0.0183   0.0115   0.0482      1.63   1.04e- 1
#> 15 CountryOf…  0.0250    0.0186   0.00636  0.0436      1.34   1.80e- 1
#> 16 CountryOf… -0.00214   0.0185  -0.0206   0.0163     -0.116  9.08e- 1
#> 17 CountryOf… -0.0208    0.0190  -0.0398  -0.00178    -1.09   2.74e- 1
#> 18 CountryOf… -0.0285    0.0188  -0.0473  -0.00974    -1.52   1.29e- 1
#> 19 CountryOf… -0.0932    0.0195  -0.113   -0.0737     -4.78   1.78e- 6
#> # ℹ 2 more variables: attribute <chr>, level <chr>
```
