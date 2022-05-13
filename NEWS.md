# frscore 0.12.1

- The unadjusted (non-normalized) fr-score of each model type is now always
included as a column named `score` in the results returned by
`frscore()` and `frscored_cna()`. A normalized score for each model type
is included as a column named `norm.score` if `normalize = "truemax"` or 
`normalize = "idealmax"`.

- A list containing a breakdown of each model's unadjusted fr-score is now 
always included in the output of `frscore()` and `frscored_cna()`. 
The `verbose` argument now only controls
whether it is printed (`verbose = TRUE`) or not (`verbose = FALSE`).


# frscore 0.1.1

- patch release with no changes that affect usage; fixes an example in `frscored_cna()` documentation that started throwing an error due to changes in the `cna` package, and stops `frscore()` from displaying a confusing warning when the number of solution types exceeds the value of the argument maxsols. 



---

# frscore 0.1.0


