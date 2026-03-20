# conjest

Basic estimands for conjoint experiments. Conjoint experiments present respondents with hypothetical profiles that vary simultaneously across multiple attributes, asking them to choose between or rate these profiles. By randomly assigning attribute values across profiles, conjoint designs exploit the logic of randomization to identify the causal effect of each attribute on choices or ratings. This makes them particularly well-suited to studying social judgments and decision-making, where multiple characteristics of a target person or object are likely to jointly influence behavior.

This package allows you to estimate & visualize:

* Average Marginal Component Effects (AMCEs), via `amce()`.
* Marginal Means, via `marginal_means()`.
* *Planned:* Average Component Interaction Effects (ACIEs).
* *Planned:* Conditional Marginal Means, for subgroup analysis.

Results work with `ggplot2::autoplot()`. Getting a look at your results is as easy as this:

```r
library(conjest)
library(ggplot2)

immigration |> 
  marginal_means(
    ChosenImmigrant ~ Education + Gender + CountryOfOrigin,
    id = ~CaseID
  ) |> 
  autoplot()
```
![](https://github.com/user-attachments/assets/e133896f-f36d-48a4-ad5e-c5a13e402421)
