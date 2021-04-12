# frscore
Functions for automating repeated CNA runs on a data set with varying consistency and coverage settings, and calculating
the fit-robustness -scores of the resulting models.

`rean_cna()` repeatedly analyzes a data set `x` with `cna` using all combinations of consistency and coverage thresholds that can be formed from values provided in the argument `attempt` --- i.e. performs a *reanalysis series* where consistency and coverage are varied within a given range at some granularity. Depending on the value of the argument `output = c("csf", "asf")`, either csfs or asfs are extracted from the results of each analysis, and these are returned in a list where each list element is a data frame of all the csfs or asfs returned with a given con/cov setting, with additional columns 'cnacon' and 'cnacov' that contain the con/cov values used in the respective `cna` run. `ncsf` can be used to control how many csfs are calculated when `output = "csf"`.
Primarily a helper function for `frscored_cna()`. Accepts additional `cna` arguments which are passed to the `cna` calls, notably, one should specify the type of input data with the `type` -argument when `x` is not binary crisp-set data.

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

`frscore()` takes as argument `sols` a character vector of `cna` solutions, and calculates for each solution a score based on how many sub-, super-, or sub- and supermodels they have in the set of solutions `sols`.  `scoretype` can be used to control what the score is based on, the default being "full", which counts both sub- and supermodel-relationships. Only unique models are printed, hence the reference to model "type" in the output. For each model type, a number of tokens, i.e. the number of times a model appears in `sols`, is printed. `verbose = TRUE` prints a breakdown of the sub/supermodel-relationships that contribute to the score of each model. The primary purpose of this function is to analyze the degree to which different models of the same data, obtained at different fit threshold settings, agree with each other in the causal ascriptions they make, using sub- and supermodel-relations as proxy for agreement in causal ascriptions between models. 
Used this way, the scores represent fit-robustness: the models with low scores compared to the others inferred with different fit thresholds either make some idiosyncratic causal ascriptions indicating overfitting, or are uninformative compared to other equally non-idiosyncratic models of the same data. 
Results are by default printed for top-20 scoring model types, use `print.all = TRUE` to print all results. The argument `normalize` controls whether the scores are normalized and if yes, how. This
defaults to "truemax", by which scores are normalized by 
the highest score obtained by any model, such that the top scoring model type(s)
always get score 1. `normalize = "idealmax"`
normalizes by a theoretical maximum score calculated by assuming that
all solutions of equal complexity are identical, and for every solution
of a given complexity, all solutions with lower complexity are its
submodels. 
The scores are normalized by default, 
as they have no absolute interpretation but are only meaningful in comparison to other models of same data obtained at different consistency/coverage settings. `frscored_cna()` automates this process (of analysing data with varying con/cov and calculating the fit-robustness of the resulting models). 




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

`frscored_cna()` performs a reanalysis series on a data set `x`by calling `rean_cna` on `x`, calculates fit-robustness for each model (type) returned (by calling `frscore()` on the resulting models), and outputs the models and their scores.
Arguments `fit.range` and `granularity` determine the reanalysis series, e.g. the defaults `fit.range = c(1, 0.7)` and `granularity = 0.1` determine that the input data set `x` is repeatedly analyzed with cna while varying consistency and coverage between 1 and 0.7 by 0.1, until all possible combinations of con/cov values within that range and that granularity of variation have been tried. Arguments `normalize`, `verbose`, and `print.all` are used similarly to `frscore()` ,e.g. `verbose = TRUE` also prints a breakdown of each model's fit-robustness score. Outputs a data frame of unique models returned in the reanalysis series, their details similarly to the output of `cna`, with additional columns 'score' and 'tokens', which display the fit-robustness score of each model (type), and the number of times. 

If a candidate model is provided as `test.model`, the result for that model will be printed separately, provided the model is found in the reanalysis series, if not,
the function stops.

The return value is an object of class "frscored_cna", which is a list that contains some additional elements to those that are printed by the print method, but may be of interest.



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
