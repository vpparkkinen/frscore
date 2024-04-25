# frscore
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/vpparkkinen/frscore/workflows/R-CMD-check/badge.svg)](https://github.com/vpparkkinen/frscore/actions)
  <!-- badges: end -->


## Overview

Functions for automatically performing a reanalysis series
on a data set using `cna::cna()`, and for calculating the fit-robustness
of the resulting models, as described in 
Parkkinen and Baumgartner (2021): https://journals.sagepub.com/doi/full/10.1177/0049124120986200.

In the most common use case, one wants to obtain a set of models and their respective fit-robustness scores given a range of consistency and coverage values that determine a reanalysis series of the data set of interest. The function `frscored_cna()` runs the reanalysis series on a data set and calculates the fit-robustness scores of the recovered models in one go. If one only wishes to repeatedly analyze a data set with different consistency and coverage thresholds in a given range, `rean_cna()` automates this. If one wishes to calculate the fit-robustness scores
for an existing set of models, or simply count (causal) sub- and supermodel relations in a set of models for any reason, `frscore()` does this.
`causal_submodel()` is a generalization of `cna::is.submodel()` that
checks whether all causal relevance ascriptions, rather than only
ascriptions of direct causation, made by one model are contained in another model. `causal_submodel()` is used by default in `frscored_cna()` and `frscore()`
to calculate fr-scores, but the user can change this to `cna::is.submodel()`
to obtain a moderate speed improvement if needed.


Have a look at the [NEWS](https://github.com/vpparkkinen/frscore/blob/main/NEWS.md) for information about recent changes and developments.

## Installation

```r
# latest version on CRAN

install.packages("frscore")
```


## Usage

```r
library(frscore)

frsc <- frscored_cna(selectCases("A+B+F*g<->R"))
frsc

rean_cna(ct2df(selectCases("A+B+F*g<->R")), attempt = seq(1, 0.7, -0.1))

res <- rean_cna(selectCases("A+B+F*g<->R"), attempt = seq(1, 0.7, -0.1))
res <- do.call(rbind, res)
fr <- frscore(res[,2])
fr

target <- "(A+B<->C)*(C+D<->E)"
candidate <- "A+B<->E"
causal_submodel(candidate, target)



```


