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

immigration |> 
  amce(ChosenImmigrant ~ Education + Gender + CountryOfOrigin, id = ~CaseID) |> 
  summary()

#> Average Marginal Component Effects
#> ============================================================ 
#> 
#> Attribute: Education 
#>  Reference level: No Formal 
#>  ------------------------------------------------------------ 
#>                   Estimate Std. Error t value Pr(>|t|)     
#>         4th Grade   0.0327     0.0155   2.109 3.50e-02  *  
#>         8th Grade   0.0549     0.0156   3.529 4.19e-04  ***
#>       High School   0.1180     0.0157   7.521 5.77e-14  ***
#>  Two-Year College   0.1761     0.0158  11.152 9.22e-29  ***
#>    College Degree   0.1994     0.0160  12.446 2.27e-35  ***
#>   Graduate Degree   0.1918     0.0159  12.037 3.32e-33  ***
#> 
#> Attribute: Gender 
#>  Reference level: Female 
#>  ------------------------------------------------------------ 
#>       Estimate Std. Error t value Pr(>|t|)     
#>  Male  -0.0241     0.0085  -2.823 4.76e-03  ** 
#> 
#> Attribute: CountryOfOrigin 
#>  Reference level: India 
#>  ------------------------------------------------------------ 
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

Results also work with `ggplot2::autoplot()`:

```r
library(ggplot2)

immigration |> 
  marginal_means(ChosenImmigrant ~ Education + Gender + CountryOfOrigin, id = ~CaseID) |> 
  autoplot()
```
![](https://github.com/user-attachments/assets/e133896f-f36d-48a4-ad5e-c5a13e402421)
