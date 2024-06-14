# frscore (development version)

<<<<<<< HEAD
=======
- new functions `submodel_adjacencies_to_igraph()` and `plot_submodel_network()`  
  for representing submodel relations as graphs, and for plotting them as networks.
  
- argument `scoretype` is removed from `frscore()` and `frscored_cna()`.

- `frscore()` and `frscored_cna()` results now include a column that describes
  the proportion of other models to which each token model relates to as
  a sub- or supermodel. 

>>>>>>> simplify
# frscore 0.4.1

- A patch release that fixes an issue caused by
changes in `cna` package.

# frscore 0.4.0

- `rean_cna()` and `frscore()` display progress tickers.

- `frscore()` and `frscored_cna()` are no longer sensitive to locale (due to
collation differences) when
number of unique model types exceeds `maxsols`. 

- unless `normalize = "none"`, `norm.score` column filled with zeroes is added to the output of `frscore()`
and `frscored_cna()` when maximum score obtained by a model is zero.

- `frscore()` and `frscored_cna()` return an adjacency matrix where
adjacencies represent submodel relations between the scored model types.

- `frscored_cna()` and  `frscore()` return how they were called.

- `frscored_cna()` and `rean_cna()` accept `output = "msc"`, in which case only mscs are calculated in, and collected
from the reanalysis series.

# frscore 0.3.1

- A new function `causal_submodel()` is introduced. `causal_submodel()` is like `cna::is.submodel()`, but checks that all
causal relevance ascriptions, rather than ascriptions of direct causation only,
of one model are contained in another model.

- `frscore()` is updated to use `causal_submodel()` instead of `cna::is.submodel()` to compare models when calculating fr-scores.

- `frscored_cna()` and `frscore()` gain an argument `comp.method` that
allows the user to decide whether fr-scores are calculated using `causal_submodel()` (default) or `cna::is.submodel()`.

- `frscore()` gains a new argument `dat`, to be used to determine
admissible factor values when processing multi-valued models using
`comp.method = "causal_submodel"`.

- `frscored_cna()` and `rean_cna()` gain a new argument `n.init` that
controls the maximum number of csfs that are calculated in each analysis.
This replaces `ncsf` in `rean_cna()`, `ncsf` is deprecated.

- The `scoretype` argument in `frscore()` and `frscored_cna()` is deprecated
ahead of removal in the next version.




# frscore 0.2.0

- The unadjusted (non-normalized) fr-score of each model type is now always
included as the `score` column in the results returned by
`frscore()` and `frscored_cna()`. A normalized score
is included as a column named `norm.score` if `normalize = "truemax"` or
`normalize = "idealmax"`.

- A list containing a breakdown of each model's unadjusted fr-score is now
always included in the output of `frscore()` and `frscored_cna()`.
The `verbose` argument now only controls
whether it is printed (`verbose = TRUE`) or not (`verbose = FALSE`).

- Print methods for `frscore()`and `frscored_cna()` have been updated
to reflect the change in `verbose` behavior, and so that objects
created by previous versions of `frscore()` and `frscored_cna()` are
still printed correctly.


# frscore 0.1.1

- patch release with no changes that affect usage; fixes an example in `frscored_cna()` documentation that started throwing an error due to changes in the `cna` package, and stops `frscore()` from displaying a confusing warning when the number of solution types exceeds the value of the argument maxsols.



---

# frscore 0.1.0
