# frscore
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/vpparkkinen/frscore/workflows/R-CMD-check/badge.svg)](https://github.com/vpparkkinen/frscore/actions)
  <!-- badges: end -->


## Overview

Functions for automatically performing a reanalysis series
on a data set using CNA, and for calculating the fit-robustness
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

# FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
# Score type: full || score normalization: truemax 
# maxsols set to 50 -- 0 model types excluded from scoring 

# ----- 
#  
# Model types: 
#  
#    outcome   condition consistency  coverage complexity inus score tokens norm.score
# 1  R       A+B+F*g<->R   1.0000000 1.0000000          4 TRUE    38      2 1.00000000
# 2  R           A+B<->R   1.0000000 0.9230769          2 TRUE    28     12 0.73684211
# 3  R         A+B+F<->R   0.9285714 1.0000000          3 TRUE    24      2 0.63157895
# 4  R         A+B+g<->R   0.9285714 1.0000000          3 TRUE    24      2 0.63157895
# .
# .
# .


rean_cna(ct2df(selectCases("A+B+F*g<->R")), attempt = seq(1, 0.7, -0.1))
# [[1]]
#   outcome         condition consistency coverage complexity inus cnacon cnacov
# 1 R       A + B + F*g <-> R           1        1          4 TRUE      1      1
# 
# [[2]]
#   outcome         condition consistency coverage complexity inus cnacon cnacov
# 1 R       A + B + F*g <-> R           1        1          4 TRUE    0.9      1
# .
# .


res <- rean_cna(selectCases("A+B+F*g<->R"), attempt = seq(1, 0.7, -0.1))
res <- do.call(rbind, res)

fr <- frscore(res[,2])
fr

# FRscore, score type: full || score normalization: truemax 
# 
# maxsols set to 50 -- 0 solution types excluded from scoring 
# 
# -----
#  
# Model types: 
# 
#          model score tokens norm.score
# 1  A+B+F*g<->R    38      2 1.00000000
# 2      A+B<->R    28     12 0.73684211
# 3    A+B+F<->R    24      2 0.63157895
# 4    A+B+g<->R    24      2 0.63157895
# 5      A+F<->R    15      4 0.39473684
# 6      A+g<->R    15      4 0.39473684
# .
# .

```


