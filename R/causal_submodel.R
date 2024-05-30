case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}


decompose_model <- function(x){
  x <- noblanks(x)
  asfs <- unlist(extract_asf(x))
  rhss <- rhs(asfs)
  lhss <- lhs(asfs)
  out <- list(asfs = asfs,
              rhss = rhss,
              lhss = lhss)
  return(out)
}


ccheck_prep <- function(x, y, ogy){
  cand_decomp <- decompose_model(x)
  cand_asfs <- cand_decomp$asfs
  cand_outs <- cand_decomp$rhss
  cand_lhss <- cand_decomp$lhss
  names(cand_lhss) <- cand_outs
  cand_facs <- unlist(lit_extract(cand_lhss), use.names = FALSE)
  not_subbable <- toupper(y$rhss) %in% c(toupper(cand_facs),
                                           toupper(cand_outs))
  test_tar_lhss <- y$lhss
  names(test_tar_lhss) <- y$rhss
  out <- list(target = ogy,
              target_lhss = test_tar_lhss,
              target_asfs = y$asfs,
              candidate = x,
              candidate_lhss = cand_lhss,
              candidate_asfs = cand_asfs,
              no_sub = not_subbable)
  return(out)
}





#'Determine if a model is a causal submodel of another model
#'@description Determine whether the causal relevance ascriptions made by
#'  \emph{candidate} solution/model \code{x} are contained in the causal
#'  relevance ascriptions made by \emph{target} model \code{y}.
#'
#'@param x A string that specifies a valid \code{cna} model.
#'
#'@param y A string that specifies a valid \code{cna} model.
#'
#'@param dat A \code{configTable}, a data frame, a matrix, or a list that
#'  specifies the range of admissible factor values for the factors featured in
#'  \code{x} and \code{y}. Only needed when the models \code{x} and \code{y} are
#'  multi-valued, otherwise ignored.
#'
#'@details \code{causal_submodel()} checks whether the causal relevance claims
#'  made by the candidate model \code{x} are contained within the causal
#'  relevance claims made by the target model \code{y}. When \code{x} and
#'  \code{y} are multi-valued models, a further argument \code{dat} must be
#'  provided to determine the admissible factor values for the factors featured
#'  in \code{x} and \code{y}. This would typically be the data set that \code{x}
#'  and \code{y} were inferred from. `causal_submodel()` is similar to, and
#'  based on [`is.submodel()`][cna::is.submodel()] from the
#'  [`cna`][cna::cna-package] package, with one important difference.
#'  [`is.submodel()`][cna::is.submodel()] checks whether a model is a syntactic
#'  submodel of another, and can thus be used to check whether all syntactically
#'  explicit causal ascriptions, i.e. claims about direct causation only, of one
#'  model are contained in another. `causal_submodel()` checks if *all* causal
#'  *relevance* claims made by `x`, i.e. claims of either direct or indirect
#'  causation, have a counterpart causal relevance ascription in `y`. In case
#'  when all causal relevance claims of `x` have a suitable (see below)
#'  counterpart in `y`, `x` is a *causal* submodel of `y`.
#'
#'  For \code{x} to be causal submodel of \code{y}, (1), every ascription of
#'  direct causal relevance made by \code{x} must either have a counterpart
#'  direct causal ascription in \code{y}, or a counterpart indirect causal
#'  ascription in \code{y} such that `x` omits any factors that mediate the
#'  relation according to `y`. (2), every ascription of indirect causal
#'  relevance made by \code{x} must have a counterpart indirect causal
#'  ascription in \code{y}. That is, every pair of factors represented as direct
#'  cause and effect in \code{x} must either be represented as direct cause and
#'  effect in \code{y}, or be connected by a transitive chain of direct causal
#'  relations according to \code{y}. In the latter case, `x` must in addition
#'  omit the factors that according to `y` mediate the causal relation in
#'  question. Direct causal relations are those causal relations that can be
#'  read off from the explicit syntax of an atomic solution/model ("asf"). For
#'  example, according to \code{A*F+B<->C}, \code{A} and \code{B} are direct
#'  causes of \code{C} on alternative paths. Furthermore, candidate model
#'  \code{A+B<->C} is a causal submodel of the target \code{A*F+B<->C}, but
#'  \code{A+B*U<->C} is not, since the latter makes a claim about the causal
#'  relevance of \code{U} to \code{C} which is not made by the target. Each
#'  direct cause is a difference-maker for its effect in some circumstances
#'  where alternative sufficient causes of the effect are not present, and the
#'  \emph{co-factors} located on the same path are present. For example,
#'  \code{A*F+B<->C} claims that when \code{B} is absent and \code{F} is
#'  present, difference in the presence of \code{A} will associate with
#'  differences in \code{C}, given some suitable configuration of factors not
#'  explicitly represented in \code{A*F+B<->C}. When both \code{x} and \code{y}
#'  are asfs, i.e. represent direct causal relations only, \code{x} is a causal
#'  submodel of \code{y} if, and only if \code{x} is is a [syntactic
#'  submodel][cna::is.submodel()] of \code{y}, as the syntax of an asf is such
#'  that every causal ascription is explicitly represented.
#'
#'  Judgments of direct vs. indirect causation are relative to the set of
#'  factors included in a model. `A+B<->E` describes `A` and `B` as direct
#'  causes of `E`, but another model that includes additional factors besides
#'  `{A,B,E}` might describe these causal relations as causal chains that
#'  include intermediate steps, as in \code{(A+B<->C)*(C+D<->E)}. `A+B<->E`
#'  makes no claim that would contradict the chain model; it merely says that
#'  *relative* to the factor set `{A,B,E}`, the factors are causally ordered so
#'  that `A` and `B` are causes of `E`, and there is no causal relation between
#'  `A` and `B`. Causal order refers to the ordering of the factors by the
#'  relation of direct causation that determines what is causally "upstream" and
#'  "downstream" of what. The exogenous factors `{A,B,D}` are top-level upstream
#'  causes in \code{(A+B<->C)*(C+D<->E)}, as they are not caused by any other
#'  factor included in the model. Endogenous factors `C` and `E` are downstream
#'  of `{A,B}` by one and two levels respectively, and `E` is one level
#'  downstream of `D`. The chain model agrees with the direct cause model on the
#'  causal ordering of `{A,B,E}` -- `A` and `B` are upstream of `E` and not
#'  causes of each other -- but also includes an additional cause of `E`, `C`,
#'  that is ordered between `{A,B}` and `E` along a chain of direct causal
#'  relations. \code{(A+B<->C)*(C+D<->E)} represents a \emph{transitive} causal
#'  chain where \code{A} and \code{B} are indirectly causally relevant for
#'  \code{E} in virtue of being causes of \code{E}'s more proximate cause
#'  \code{C} and the difference-making ability they have on \code{E} via
#'  \code{C}. \code{A+B<->E} is a causal submodel of \code{(A+B<->C)*(C+D<->E)},
#'  as the models agree on the causal relevance ascriptions over `{A,B,E}`, and
#'  the former makes no claims whatsoever about the additional factors `{C,D}`
#'  included in the latter model. Both models can be seen as descriptions of the
#'  same causal structure, one more complete in detail than the other. An
#'  \emph{in}transitive chain is a causal chain where the influence of some
#'  upstream causes is not transmitted to some downstream effects. For example,
#'  \code{(A+B<->C)*(C*a+D<->E)} represents a chain where \code{A} is not
#'  causally relevant to \code{E} despite being a cause of one of \code{E}'s
#'  direct causes (\code{C}). That is, according to this model, \code{A} is not
#'  a difference-maker for \code{E}, and \code{A+B<->E}, which makes this claim,
#'  is not its causal submodel.
#'
#'  Besides avoiding causal relevance ascriptions that are not present in the
#'  target at all, the candidate should also attribute causal relevance
#'  correctly in the sense of causally ordering the represented causes in a way
#'  that is compatible with the target. Factors that appear as direct causes of
#'  the same outcome both in the target and the candidate should be grouped into
#'  alternative disjuncts similarly in both. Analogously, causes that appear on
#'  different levels in a causal chain according to the target should not be
#'  represented as same-level causes by the candidate. Say, for example, that
#'  the target is \code{(A+B<->C)*(C+D<->E)*(E+F<->G)}. Candidate models
#'  \code{(A+B<->G)} and \code{(A+B<->E)*(E+F<->G)} are both causal submodels of
#'  this target. By contrast, neither of \code{(A+C<->G)} and
#'  \code{(A+B<->C)*(C+E<->G)} is a causal submodel of the target. Both of the
#'  latter two models commit the error of representing as same-level causes
#'  factors that the target represents as cause and effect. For example,
#'  \code{(A+C<->G)} claims that \code{A} and \code{C} are same-level causes of
#'  \code{G}, whereas the target says \code{A} is a cause of \code{C}. In other
#'  words, relative to a factor set that include `C`, the candidate claims that
#'  `A` is a direct cause of `E`, which is false according to the target. It is
#'  instructive to consider the difference in implications for
#'  difference-making: \code{(A+C<->G)} claims that differences in \code{A}
#'  associate with differences in \code{G} when \code{C} is fixed absent, but
#'  the target claims that this is impossible.
#'
#'  Finally, a causal submodel relation requires that any claims of indirect
#'  causal relevance made by a candidate model are claims made by the target
#'  also. Consider the target model \code{(A+B*D<->C)*(C+D<->G)} and a candidate
#'  \code{(A+B*D<->C)*(C<->G)}. Despite superficial similarity (the candidate is
#'  a syntactic submodel of the target), the candidate is not a causal submodel
#'  of the target. Namely, the candidate makes a claim that \code{B} is
#'  indirectly causally relevant for \code{G}, a claim that is not made by the
#'  target. Again, it is best to examine the specific difference-making claim in
#'  question. The candidate model claims that differences in \code{B} make a
#'  difference to the presence of \code{G} when \code{D} is fixed to be present.
#'  But this is false according to the target. The target claims that \code{G}
#'  is always present whenever \code{D} is: \code{B} is not causally relevant
#'  for \code{G} despite being a cause of an intermediary factor \code{C}.
#'
#'  In its implementation, \code{causal_submodel()} relies on the fact that when
#'  both the target and candidate are asfs, a syntactic submodel relation that
#'  can be verified with [`is.submodel()`][cna::is.submodel] is a necessary and
#'  sufficient condition for causal submodel relation. If both the candidate and
#'  the target are asfs, a check for syntactic submodel relation is performed,
#'  and the result returned. When the target, or both the target and candidate
#'  comprise more than one asf, the process is more complicated. First,
#'  `causal_submodel()` checks if the component asfs of the candidate are
#'  syntactic submodels of the target \emph{as is}. If yes for all, each of the
#'  candidate's direct causal relevance ascriptions is contained in the target,
#'  and the function proceeds to the second phase. For those direct causal
#'  relations that are not contained in the target, the function searches for
#'  counterpart indirect relations in the target. Since \code{cna} models do not
#'  represent indirect relations explicitly, these are explicated by
#'  syntactically manipulating the target. This involves finding asfs in the
#'  target with the same outcomes as those candidate asfs that are not syntactic
#'  submodels of the target \emph{as is}. For each such component asf of the
#'  target, factors in the disjunction on the left hand side of the equivalence
#'  sign ("<->") are substituted with the disjunctions, if any, that according
#'  to the target represent their causes. The resulting expression is then
#'  minimized to render it causally interpretable. What is left is an asf
#'  representing some of the target's indirect causal claims as direct causal
#'  claims. Then, the candidate asfs that are not syntactic submodels of the
#'  target \emph{as is} are tested against the manipulated target asfs for
#'  syntactic submodel relation. This process is repeated until all the submodel
#'  checks return `TRUE`, or no further substitutions are possible. In the
#'  former case, the function proceeds to the second phase. In the latter case,
#'  the candidate is deemed not to be a causal submodel of the target, and the
#'  function returns `FALSE`.
#'
#'  An example is in order to illustrate the procedure so far. Say that the
#'  target and candidate are \code{(A+B<->C)*(C+D<->E)} and \code{A+B<->E},
#'  respectively. Since the sole candidate asf is not a syntactic submodel of
#'  the target, one then attempts to find indirect causal relevance ascriptions
#'  in the target to license the direct causal claims made by the candidate asf.
#'  By the procedure described above, one focuses on the second asf of the
#'  target, \code{C+D<->E}, and seeks to syntactically manipulate that until it
#'  is transformed into a syntactic supermodel of \code{A+B<->E}, or until no
#'  transformation is possible. According to the first component asf of the
#'  target, \code{C} is equivalent to (caused by) \code{A+B}. Hence, \code{C} in
#'  \code{C+D<->E} can be replaced with \code{A+B}, which yields
#'  \code{(A+B)+D<->E}, reducing simply to \code{A+B+D<->E}. Since
#'  \code{A+B<->E} is a syntactic submodel of \code{A+B+D<->E}, we have shown
#'  that the causal relevance claims made by the candidate are contained in the
#'  target.
#'
#'  The purpose of the second phase is to check that all indirect causal claims
#'  made by the \emph{candidate} model have a counterpart in the target. This
#'  involves doing all the substitutions of left-hand side factors by their
#'  causes in the candidate model, to generate expressions that explicitly
#'  represent the indirect claims of the candidate. The asfs generated by such
#'  manipulations of the candidate model are then tested for causal
#'  compatibility with the target, following the exact same procedure described
#'  above. For example, say that \code{(A+B*D<->C)*(C+D<->G)} and
#'  \code{(A+B*D<->C)*(C<->G)} are the target and the candidate, respectively.
#'  Here, each candidate asf \code{A+B*D<->C} and \code{C<->G} has a supermodel
#'  in one of the target asfs \code{A+B*D<->C} and \code{C+D<->G}, i.e. each
#'  direct causal claim of the candidate has a counterpart direct causal claim
#'  in the target, and the function proceeds to the second phase. In the second
#'  phase, the indirect causal claims of the candidate are first made explicit.
#'  By substituting \code{A+B*D} in place of \code{C} in the second asf of the
#'  candidate and minimizing, one gets \code{A+B*D<->G}, which represents the
#'  indirect causal relevance, as claimed by the candidate, of \code{A+B*D} on
#'  \code{G}. This expression is then tested against the target as in the first
#'  phase: the target asf with \code{G} as the outcome is manipulated to reflect
#'  the indirect claims that the target makes about \code{G}, based on what the
#'  target says about the indirect causes of `G`. After substitution and
#'  minimization, we get \code{A+D<->G}, meaning that the target does \emph{not}
#'  make a claim of indirect causal relevance of \code{B} for \code{G}. That the
#'  candidate's indirect causal ascriptions are not contained in the target is
#'  shown by the fact that \code{A+B*D<->G} is not a syntactic submodel of
#'  \code{A+D<->G}, and the function returns `FALSE`.
#'
#'  Due to the computational demands of some of the steps in the above
#'  procedure, \code{causal_submodel()} is an approximation of a strictly
#'  speaking valid check for causal submodel relations. Since the syntactic
#'  manipulations and especially the minimization of the resulting expressions
#'  is so costly, \code{causal_submodel()} relies on the
#'  [`rreduce()`][cna::rreduce()] function from the [`cna`][cna::cna-package]
#'  package for minimization. [`rreduce()`][cna::rreduce()] randomly chooses a
#'  single reduction path to produce only one minimal form of an expression
#'  whenever more than one exists, i.e. when the expression is ambiguous in its
#'  causal claims. In the case of ambiguous models, the output of
#'  \code{causal_submodel()} may depend on which reduction path(s) were chosen.
#'  These cases are rare enough to not significantly affect the intended use of
#'  \code{causal_submodel()} in the context of `frscore`. Another instance of
#'  \code{causal_submodel()} taking a shortcut is when processing cyclic models
#'  like \code{(A+B<->C)*(C+D<->A)}. Here the problems are as much philosophical
#'  as computational. It is clear that a cyclic candidate model cannot be a
#'  causal submodel of a non-cyclic target. However, problems arise when testing
#'  a non-cyclic candidate against a cyclic target: it is not clear what counts
#'  as an incompatibility in causal ordering, given that a cyclic target model
#'  includes factors that are causally relevant for themselves. Since many
#'  conclusions can be argued for here but some approach must be taken to ensure
#'  that \code{causal_submodel()} works on all valid \code{cna} models,
#'  \code{causal_submodel()} takes the least costly option and simply checks
#'  whether the candidate is a syntactic submodel of the target, and returns the
#'  result.
#'@returns Named logical.
#'@seealso [cna::is.submodel()]
#'
#'@examples
#'target <- "(A+B<->C)*(C+D<->E)"
#'candidate1 <- "A+B<->E"
#'causal_submodel(candidate1, target) # TRUE
#'candidate2 <- "A+C<->E"
#'causal_submodel(candidate2, target) # FALSE
#'
#'dat <- cna::d.pban
#'target_mv <- "C=1 + F=2 + T=1 + C=0*F=1 <-> PB=1"
#'candidate_mv <- "C=1 + F=2 + T=1 <-> PB=1"
#'causal_submodel(candidate_mv, target_mv, dat = dat) # mv models require the 'dat' argument
#'
#'@importFrom cna C_is_submodel cyclic extract_asf full.ct getCond hstrsplit lhs
#'@importFrom cna noblanks relist1 rhs rreduce selectCases
#'@importFrom stats setNames
#'@export
causal_submodel <- function(x, y, dat = NULL){
  x <- noblanks(x)
  y <- noblanks(y)
  out <- vector("logical", 1)
  attributes(out) <- list(target = y)
  names(out) <- x
  mvx <- grepl("=", x)
  mvy <- grepl("=", y)
  if(xor(mvx, mvy)){stop("x and y must be of same model type")}
  if(mvy){
    type <- "mv"
  } else {
    type <- "bin"
  }
  if(is.null(dat) & type == "mv"){
    stop("A data frame, configTable, or a list specifying admissible \n
         factor values must be provided for multi-valued models")
  }
  if(type == "bin") {dat <- full.ct(y)}
  if(type == "mv") {dat <- full.ct(dat)}
  is_sm <- fsubmodel_csf(x,y)
  c_asfcount <- unlist(strsplit(x, "<->"))
  is_x_csf <- length(c_asfcount) > 2
  if(!is_x_csf & is_sm){
    out[1] <- TRUE
    return(out)
  }
  if(cyclic(x) || cyclic(y)){
    out[1] <- is_sm
    return(out)
  }
  dy <- decompose_model(y)
  out[1] <- subin_target_ccomp(x =x, y = dy, ogy = y, dat = dat, type = type)
  if(!out){
    return(out)
  }
  x <- is_comp_subtar(x, dat = dat, type = type)
  out[1] <- subin_target_ccomp(x =x, y = dy, ogy = y, dat = dat, type = type)
  return(out)
}

subin_target_ccomp <- function(x, y, ogy, dat = NULL, type){
  prepared <- ccheck_prep(x, y, ogy)
  prep_target <- prepared$target_lhss
  asf_subms <- fsubmodel_csf(prepared$candidate_asfs, ogy)
  cand_need_checking <- prepared$candidate_lhss[!asf_subms]
  subbed_tar_asfs <- vector("character", length(prepared$target_lhss))
  correct <- asf_subms
  names(correct) <- prepared$candidate_asfs

  for(i in seq_along(cand_need_checking)){
    subbed_tar_asfs[i] <- check_comp_asf(cand_need_checking[i],
                                         prepared$target_lhss,
                                         prepared$no_sub,
                                         #y,
                                         ogy,
                                         dat = dat,
                                         type = type)
    idx <- which(names(prepared$candidate_lhss) == names(cand_need_checking[i]))
    asf_cor <- fsubmodel_asf(prepared$candidate_asfs[idx],
                           subbed_tar_asfs[i])
    correct[names(correct) == prepared$candidate_asfs[idx]] <- asf_cor
  }
  out <- all(correct)
  return(out)
}

extr_mv_facs <- function(x){
  fnames <- unlist(strsplit(x,
                            "\\(|\\)|\\+|\\*|\\="))
  fnames <- fnames[sapply(fnames, function(x) grepl("\\D", x))]
  out <- unique(fnames)
  return(out)
}


check_comp_asf <- function(x, y, not_subbable, ogy, dat = NULL, type){
  if(type == "bin" & is.null(dat)){
    dat <- full.ct(ogy)
  }
  tar_lhss <- y
  tar_outs <- names(y)
  tar_outs_flipped <- sapply(tar_outs, case_flipper)
  cand_out <- names(x)
  outc_matches <- tar_outs %in% cand_out
  if(!any(outc_matches)){
    return(ogy)
  }
  outcome_match <- which(outc_matches)

  ultimate_lhs <- ultimate_lhs1 <- y[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))

  while(any(sapply(toupper(tar_outs[!not_subbable]),
                   function(x) fac_grepl(x, toupper(ultimate_lhs))))){
    sub_from_capmatch <- sapply(tar_outs,
                                function(y)
                                  fac_grepl(y, ultimate_lhs))
    sub_from_capmatch[which(not_subbable)] <- FALSE
    sub_from_capflip <- sapply(tar_outs_flipped,
                               function(y)
                                 fac_grepl(y, ultimate_lhs))
    sub_from_capflip[which(not_subbable)] <- FALSE
    subbing_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip)))
    idx_sub_from[subbing_from] <- TRUE
    if(any(sub_from_capmatch)){
      for(i in which(sub_from_capmatch)){
        ultimate_lhs <- gsub(paste0("(?<![[:alpha:]])", tar_outs[i], "(?!\\w)"),
                             paste0("(", tar_lhss[i], ")"),
                             ultimate_lhs,
                             perl = TRUE)
      }
    }
    if(any(sub_from_capflip)){
      for(i in which(sub_from_capflip)){
        ultimate_lhs <- gsub(paste0("(?<![[:alpha:]])",
                                    tar_outs_flipped[i],
                                    "(?!\\w)"),
                            paste0("!(", tar_lhss[i], ")"),
                            ultimate_lhs,
                            perl = TRUE)
      }
    }
  }

  if(identical(ultimate_lhs, ultimate_lhs1)){
    subbed_lhs <- ultimate_lhs1
  } else {
    if(type == "mv"){
      cond <- get_mvcond(lhs = ultimate_lhs, dat = dat)
    } else {
      cond <- getCond(selectCases(ultimate_lhs))
    }
    subbed_lhs <- rreduce(cond = cond,
                          x = selectCases(ogy, x = dat),
                          full = FALSE)
  }
  out <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  return(out)
}

get_mvcond <- function(lhs, dat){
  fnames <- extr_mv_facs(lhs)
  u_lhsdat <- selectCases(cond = lhs,
                          x = dat[,fnames])
  cond <- getCond(u_lhsdat)
  return(cond)
}


substitute_all <- function(x, dat = NULL, type){
  x <- decompose_model(x)
  subbed <- chain_substituter(x)
  lhss <- subbed$lhss
  if(type == "mv"){
    subbed$lhss <- unlist(lapply(lhss, function(x) get_mvcond(x, dat)))
  } else {
    subbed$lhss <- unlist(lapply(lhss, function(z) getCond(selectCases(z))))
  }
  return(subbed)
}


fac_grepl <- function(te, tar){
  r <- paste0("(?<![[:alpha:]])", te, "(?!\\w)")
  out <- grepl(r, tar, perl = TRUE)
  return(out)
}


chain_substituter <- function(x,
                               subbed_from = vector("logical", length(x[[1]]))){
  sub_from_capmatch <- lapply(x$rhss,
                              function(y) fac_grepl(y, x$lhss))
  id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
  sub_from_capflip <- lapply(case_flipper(x$rhss),
                             function(y) fac_grepl(y, x$lhss))
  id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  while(any(c(id_sub_capflip, id_sub_capmatch))){
    for(i in seq_along(sub_from_capmatch)){
      if(id_sub_capmatch[i]){
        x$lhss[sub_from_capmatch[[i]]] <- gsub(paste0("(?<![[:alpha:]])",x$rhss[i],"(?!\\w)"),
                                               paste0("(", x$lhss[i], ")"),
                                               x$lhss[sub_from_capmatch[[i]]],
                                               perl = TRUE)
      }
    }
    for(i in seq_along(sub_from_capflip)){
      if(id_sub_capflip[i]){
        x$lhss[sub_from_capflip[[i]]] <- gsub(paste0("(?<![[:alpha:]])", tolower(x$rhss[i]),"(?!\\w)"),
                                              paste0("!(", x$lhss[i], ")"),
                                              x$lhss[sub_from_capflip[[i]]],
                                              perl = TRUE)
      }
    }

    wh_subbed_from <- which(subbed_from)
    wh_capmatch <- which(id_sub_capmatch)
    wh_capflip <- which(id_sub_capflip)
    w_subbed <- unique(c(wh_subbed_from, wh_capmatch, wh_capflip))
    subbed_from[w_subbed] <- TRUE
    sub_from_capmatch <- lapply(x$rhss,
                                function(y) fac_grepl(y, x$lhss))
    id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
    sub_from_capflip <- lapply(case_flipper(x$rhss),
                               function(y) fac_grepl(y, x$lhss))
    id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  }
  x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
  return(x)
}


is_comp_subtar <- function(x, y, type, dat){
  x_expanded <- substitute_all(x, dat, type)
  x_expanded$lhss <- unlist(lapply(x_expanded$lhss,
                                   function(z) rreduce(cond = z, x = dat)))
  d <- data.frame(x_expanded$lhss, x_expanded$rhss)
  new_asfs <- do.call("paste", c(d, sep = "<->"))
  if(length(new_asfs) > 1){
    new_asfs <- paste0("(", new_asfs, ")")
  }
  new_csf <- paste0(new_asfs, collapse = "*")
  return(new_csf)
}


mvdatgen <- function(x){
  fct <- full.ct(x)
  fct_u <- apply(fct, 2, unique)
  mv_values <- lapply(fct_u,
                      function(x) {if(length(unique(x)) < 3){
                        x <- min(x):(max(x)+(3-length(x)))
                      } else {
                        x <- x
                      }
                        return(x)})
  out <- full.ct(x = mv_values)
  return(out)
}

lit_extract <- function(lhs){
  d <- strsplit(lhs, "\\+")
  out <- lapply(d, function(x) strsplit(x, "\\*"))
  return(out)
}

















