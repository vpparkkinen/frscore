# frscore
Functions for automating repeated CNA runs on a data set with varying consistency and coverage settings, and calculating
the fit-robustness -scores of the resulting models.

`rean_cna()` repeatedly analyzes a data set `x` with `cna` using all combinations of consistency and coverage thresholds that can be formed from values provided in the argument `attempt` --- i.e. performs as *reanalysis series* where consistency and coverage are varied within a given range at some granularity --- extracts asfs or csfs, as determined by argument `output`, from the output of each analysis, and returns these in a list where each list element contains all the csfs or asfs returned with a given con/cov setting, with additional columns 'cnacon' and 'cnacov' for the con/cov values used in the respective `cna` run. `ncsf` can be used to control how many csfs are calculated when `output = "csf"`.
Primarily a helper function for `frscored_cna()`. Accepts additional `cna` arguments, which are passed to the `cna` calls within the function.

```
res <- rean_cna(selectCases("A+B+F*g<->R"), attempt = seq(1, 0.7, -0.1), output = "csf")
res

# [[1]]
# outcome         condition consistency coverage complexity inus cnacon cnacov
# 1 R       A + B + F*g <-> R           1        1          4 TRUE      1      1
# 
# [[2]]
# outcome         condition consistency coverage complexity inus cnacon cnacov
# 1 R       A + B + F*g <-> R           1        1          4 TRUE    0.9      1
# 
# [[3]]
# outcome       condition consistency coverage complexity inus cnacon cnacov
# 1 R       A + B + F <-> R   0.9285714        1          3 TRUE    0.8      1
# 2 R       A + B + g <-> R   0.9285714        1          3 TRUE    0.8      1
# .
# .
# .
#
```

`frscore()` takes as argument `sols` a character vector of cna solutions, and calculates for each solution a score based on how many sub-, super-, or sub- and supermodels they have in that set of solutions.  `scoretype` can be used to control what the score is based on, the default being "full", which counts both sub- and supermodel-relationships. Only unique models are printed, hence the reference to model "type". For each type, a number of tokens, i.e. the number of times a model appears in `sols`, is printed. `verbose = TRUE` prints a breakdown of the sub/supermodel-relationships that contribute to the score of each model. The primary purpose of this function is to analyze the degree to which different models of the same data, obtained at different fit threshold settings, agree with each other in the causal ascriptions they make, using sub- and supermodel-relations as proxy for agreement in causal ascriptions between two models.
Used this way, the scores represent fit-robustness: the models with low scores compared to the others inferred with different fit thresholds either make some idiosyncratic causal ascriptions indicating overfitting, or are uninformative compared to other equally non-idiosyncratic models of the same data. 
Results are by default printed for top-20 scoring model types, use `print.all = TRUE` to print all results. The argument `normalize` controls whether the scores are normalized or not, defaulting to `TRUE` (normalized scores), as the scores have no absolute interpretation.

```
res <- rean_cna(selectCases("A+B+F*g<->R"), attempt = seq(1, 0.7, -0.1))
res <- do.call(rbind, res)

fr <- frscore(res[,2])
fr

# FRscore, score type: full 
# -----
#   
#   Model types: 
#   
#   model              score      tokens
# 1  A + B + F*g <-> R 1.00000000      2
# 2  A + B <-> R       0.73684211     12
# 3  A + B + F <-> R   0.63157895      2
# 4  A + B + g <-> R   0.63157895      2 
# .
# .
# .
```

`frscored_cna()` performs a reanalysis series on a data set (by calling `rean_cna`), calculates fit-robustness for each model (type) returned (by calling `frscore()` on the resulting models), and outputs the models and their scores.
Arguments `fit.range` and `granularity` determine the reanalysis series, e.g. the defaults `fit.range = c(1, 0.7)` and `granularity = 0.1` determine that the input data set `x` is repeatedly analyzed with cna while varying consistency and coverage between 1 and 0.7 by 0.1, until all possible combinations of con/cov values within that range and that granularity of variation have been tried. Arguments `normalize`, `verbose`, and `print.all` are used similarly to `frscore()` (e.g. `verbose = TRUE` also returns a breakdown of each model's fit-robustness score). Returns a data frame of unique models returned and their details as in output of `cna`, with additional columns 'score' and 'tokens'. 

If a candidate model is provided as `test.model`, result for that model will be printed separately
(assuming one uses print.frscored_cna()), provided the model is found in the reanalysis series, if not,
the function stops.

```
frsc <- frscored_cna(ct2df(selectCases("A+B+F*g<->R")))
frsc

# FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
# Score type: full 
# ----- 
#   
#   Model types: 
#   
#   outcome   condition consistency  coverage complexity inus      score  tokens
# 1  R       A+B+F*g<->R   1.0000000 1.0000000          4 TRUE 1.00000000      2
# 3  R         A+B+F<->R   0.9285714 1.0000000          3 TRUE 0.63157895      2
# 4  R         A+B+g<->R   0.9285714 1.0000000          3 TRUE 0.63157895      2
# .
# .
# .

frsc2 <- frscored_cna(ct2df(selectCases("A+B+F*g<->R")), test.model = "A+f<->R")
frsc2

# FR-scored reanalysis series with fit range 1 to 0.7 with granularity 0.1 
# Score type: full 
# ----- 
 
# Candidate model tested: A+f<->R 
 
#    outcome condition consistency  coverage complexity inus      score tokens
# 50 R         A+f<->R   0.8333333 0.7692308          2 TRUE 0.05263158      1
```
