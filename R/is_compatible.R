library(cna)


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
  # tar_decomp <- decompose_model(y)

  cand_decomp <- decompose_model(x)
  #tar_asfs <- tar_decomp$asfs
  #tar_outs <- tar_decomp$rhss
  #tar_lhss <- tar_decomp$lhss
  cand_asfs <- cand_decomp$asfs
  cand_outs <- cand_decomp$rhss
  cand_lhss <- cand_decomp$lhss
  names(cand_lhss) <- cand_outs
  cand_facs <- unlist(lit_extract(cand_lhss), use.names = FALSE)
  # not_subbable <- toupper(tar_outs) %in% c(toupper(cand_facs),
  #                                          toupper(cand_outs))
  not_subbable <- toupper(y$rhss) %in% c(toupper(cand_facs),
                                           toupper(cand_outs))
  # test_tar_lhss <- tar_lhss
  test_tar_lhss <- y$lhss
  # names(test_tar_lhss) <- tar_outs
  names(test_tar_lhss) <- y$rhss
  # out <- list(target = y,
  #             target_lhss = test_tar_lhss,
  #             target_asfs = tar_asfs,
  #             candidate = x,
  #             candidate_lhss = cand_lhss,
  #             candidate_asfs = cand_asfs,
  #             no_sub = not_subbable)
  out <- list(target = ogy,
              target_lhss = test_tar_lhss,
              target_asfs = y$asfs,
              candidate = x,
              candidate_lhss = cand_lhss,
              candidate_asfs = cand_asfs,
              no_sub = not_subbable)
  return(out)
}





#' Determine the causal compatibility of `cna` models
#' @description Determine whether the causal ascriptions made by
#'   \emph{candidate} solution/model \code{x} are compatible with the causal
#'   ascriptions made by \emph{target} model \code{y}.
#'
#' @param x A string that specifies a valid \code{cna} model.
#'
#' @param y A string that specifies a valid \code{cna} model.
#'
#' @param dat A \code{configTable}, a data frame, a matrix, or a list that
#'   specifies the range of admissible factor values for the factors featured in
#'   \code{x} and \code{y}. Only needed when the models \code{x} and \code{y}
#'   are multi-valued, otherwise ignored.
#'
#' @details \code{is_compatible()} checks whether the causal relevance claims
#'   made by the candidate model \code{x} are compatible with the causal
#'   structure represented by the target model \code{y}. In short, the function
#'   checks whether \code{x} is causally compatible with \code{y}. When \code{x}
#'   and \code{y} are multi-valued models, a further argument \code{dat} must be
#'   provided to determine the admissible factor values for the factors featured
#'   in \code{x} and \code{y}. This would typically be the data set that
#'   \code{x} and \code{y} were inferred from.
#'
#'   For \code{x} to be causally compatible with \code{y}, (1), every ascription
#'   of direct causal relevance made by \code{x} must have a counterpart direct
#'   causal ascription in \code{y}, or a counterpart indirect causal ascription
#'   in \code{y} such that any factors that mediate the relation in `y` (making
#'   it indirect in `y`) are omitted in `x`. (2), every ascription of indirect
#'   causal relevance made by \code{x} must have a counterpart indirect causal
#'   ascription in \code{y}. That is, every pair of factors represented as
#'   direct cause and effect in \code{x} must either be represented as direct
#'   cause and effect in \code{y}, or must correspond to a transitive chain of
#'   direct causal relations according to \code{y} such that `x` omits the
#'   factors that mediate the relation in `y`. Direct causal relations are those
#'   causal relations that can be read off from the explicit syntax of an atomic
#'   solution/model ("asf"), and is thus relativized to the set of factors
#'   included into a model. For example, according to \code{"A*F+B<->C"},
#'   \code{A} and \code{B} are direct causes of \code{C} on alternative paths.
#'   Furthermore, candidate model \code{"A+B<->C"} is causally compatible with
#'   the target \code{"A*F+B<->C"}, but \code{"A+B*U<->C"} is not, since the
#'   latter makes a claim about the causal relevance of \code{U} to \code{C}
#'   which is not made by the target. Each direct cause is a difference-maker
#'   for its effect in some circumstances where alternative sufficient causes of
#'   the effect are not present, and the \emph{co-factors} located on the same
#'   path are present. For example, \code{"A*F+B<->C"} claims that when \code{B}
#'   is absent and \code{F} is present, difference in the presence of \code{A}
#'   will associate with differences in \code{C}, given some suitable
#'   configuration of factors not explicitly represented in \code{"A*F+B<->C"}.
#'   When \code{x} and \code{y} are atomic, i.e. represent direct causal
#'   relations only, a necessary and sufficient condition for causal
#'   compatibility is that \code{x} is a [submodel][cna::is.submodel()] of
#'   \code{y}.
#'
#'   A causal chain comprises sequentially ordered direct causal relations. For
#'   example, \code{"(A+B<->C)*(C+D<->E)"} represents a \emph{transitive} causal
#'   chain where \code{A} and \code{B} are indirectly causally relevant for
#'   \code{E} in virtue of being causes \code{E}'s more proximate cause \code{C}
#'   and the difference-making ability they have on \code{E} via \code{C}. A
#'   model like \code{"A+B<->E"} is causally compatible with
#'   \code{"(A+B<->C)*(C+D<->E)"} even though they make different claims about
#'   direct vs. indirect causal relevance. Direct causation is a model-relative
#'   notion, so this difference in itself does not entail incompatibility, given
#'   that the simpler model does not include the mediating factor `C` at all.
#'   Both models entail that \code{A} and \code{B} are difference-makers for
#'   \code{E} in some circumstances, and hence causally relevant for \code{E}.
#'   The models are thus compatible;
#'   the simpler one merely omits the middle link \code{C} which transmits the
#'   causal influence of \code{A} and \code{B} to \code{E}. Hence, both models
#'   can be seen as descriptions of the same causal structure, one more complete
#'   in detail than the other.
#'
#'
#'   An \emph{in}transitive chain is a causal chain where the influence of some
#'   upstream causes is not transmitted to some downstream effects. For example,
#'   \code{"(A+B<->C)*(C*a+D<->E)"} represents a chain where \code{A} is not
#'   causally relevant to \code{E} despite being a cause of one of \code{E}'s
#'   direct causes (\code{C}). That is, according to this model, \code{A} is not
#'   a difference-maker for \code{E}, and \code{"A+B<->E"}, which makes this
#'   claim, is not causally compatible with it.
#'
#'   Besides avoiding causal relevance ascriptions that are not present in the
#'   target at all, the candidate should also attribute causal relevance
#'   correctly in the sense of ordering the represented causes in a way that is
#'   compatible with the target. Factors that appear as direct causes of the
#'   same outcome both in the target and the candidate should be grouped into
#'   alternative disjuncts similarly in both. Analogously, causes that appear on
#'   different levels or steps in a causal chain according to the target should
#'   not be represented as same-level causes by the candidate. Say, for example,
#'   that the target is \code{"(A+B<->C)*(C+D<->E)*(E+F<->G)"}. Candidate models
#'   \code{"(A+B<->G)"} and \code{"(A+B<->E)*(E+F<->G)"} are both compatible
#'   with this target. By contrast, neither of \code{"(A+C<->G)"} and
#'   \code{"(A+B<->C)*(C+E<->G)"} is compatible with the target. Both of the
#'   latter two models commit the error of representing as same-level causes
#'   factors that the target represents as cause and effect. For example,
#'   \code{"(A+C<->G)"} claims that \code{A} and \code{C} are same-level causes
#'   of \code{G}, whereas the target says \code{A} is a cause of \code{C}. That
#'   this is a conflict in causal relevance ascription is easiest to see by
#'   considering the implications for difference-making: \code{"(A+C<->G)"}
#'   claims that differences in \code{A} associate with differences in \code{G}
#'   when \code{C} is fixed absent, but the target claims that this is
#'   impossible.
#'
#'   Finally, causal compatibility requires that any claims of indirect causal
#'   relevance made by a candidate model are claims made by the target also.
#'   Consider the target model \code{"(A+B*D<->C)*(C+D<->G)"} and a candidate
#'   \code{"(A+B*D<->C)*(C<->G)"}. Despite superficial similarity (the candidate
#'   is a submodel of the target), the candidate is incompatible with the
#'   target. Namely, the candidate makes a claim that \code{B} is indirectly
#'   causally relevant for \code{G}, a claim that is not made by the target.
#'   Again, it is best to examine the specific difference-making claim in
#'   question. The candidate model claims that differences in \code{B} when
#'   \code{D} is fixed to be present make a difference to the presence of
#'   \code{G}. But this is false according to the target. The target claims that
#'   \code{G} is always present whenever \code{D} is: \code{B} is not causally
#'   relevant for \code{G} despite being a cause of an intermediary factor
#'   \code{C}.
#'
#'   In its implementation, \code{is_compatible} exploits the fact that a
#'   submodel relation is a necessary and sufficient condition for causal
#'   compatibility of two atomic models. First, the function checks if the
#'   component asfs of the candidate are submodels of the target \emph{as is}.
#'   If yes for all, each of the candidate's direct causal relevance ascriptions
#'   has a counterpart direct causal relation in the target, and the function
#'   proceeds to the second phase. For those direct relations that cannot be
#'   mapped to direct relations as represented by the target, the function
#'   searches for counterpart indirect relations in the target. Since \code{cna}
#'   models do not represent indirect relations explicitly, these must be
#'   explicated by syntactically manipulating the target. This involves finding
#'   asfs in the target with the same outcomes as those candidate asfs that are
#'   not submodels of the target \emph{as is}. For each such component asf of
#'   the target, factors in the disjunction on the left hand side of the
#'   equivalence sign ("<->") are substituted with the disjunctions, if any,
#'   that according to the target represent their causes. The resulting
#'   expression is then minimized to render it causally interpretable. What is
#'   left is an atomic model representing some of the target's indirect causal
#'   claims as direct causal claims. Then, the candidate asfs that are not
#'   submodels of the target \emph{as is} are tested against the manipulated
#'   target asfs for submodel relation. This process is repeated until all the
#'   submodel checks return TRUE, or no further substitutions are possible. In
#'   the former case, the function proceeds to the second phase. In the latter
#'   case, the candidate is deemed not causally compatible with the target, and
#'   the function returns FALSE.
#'
#'   An example is in order to illustrate the procedure so far. Say that the
#'   target and candidate are \code{"(A+B<->C)*(C+D<->E)"} and \code{"A+B<->E"},
#'   respectively. Since the only candidate asf is not a submodel of the target,
#'   one then attempts to find indirect causal relevance claims in the target to
#'   license the direct causal claims made by that asf. By the procedure
#'   described above, one focuses on the second asf of the target,
#'   \code{"C+D<->E"}, and seeks to syntactically manipulate that until it is
#'   transformed into a supermodel of \code{"A+B<->E"}, or until no
#'   transformation is possible. According to the first component asf of the
#'   target, \code{C} is equivalent to (caused by) \code{A+B}. Hence, \code{C}
#'   in \code{"C+D<->E"} can be replaced with \code{A+B}, which yields
#'   \code{"(A+B)+D<->E"}, reducing simply to \code{"A+B+D<->E"}. Since
#'   \code{"A+B<->E"} is a submodel of \code{"A+B+D<->E"}, we have shown that
#'   the target makes claims of indirect causal relevance that license the
#'   (direct) causal claims made by the candidate.
#'
#'   The purpose of the second phase is to check that all indirect causal claims
#'   made by the \emph{candidate} model have a counterpart in the target. This
#'   involves doing all the substitutions of left-hand side factors by their
#'   causes in the candidate model, to generate expressions that explicitly
#'   represent the indirect claims of the candidate. The asfs generated by such
#'   manipulations of the candidate model are then checked against the target
#'   similarly to the first phase. For example, say that
#'   \code{"(A+B*D<->C)*(C+D<->G)"} and \code{"(A+B*D<->C)*(C<->G)"} are the
#'   target and the candidate, respectively. Here, each candidate asf
#'   \code{"A+B*D<->C"} and \code{"C<->G"} has a supermodel in one of the target
#'   asfs \code{"A+B*D<->C"} and \code{"C+D<->G"}, so the function proceeds to
#'   the second phase. In the second phase, the indirect causal claims of the
#'   candidate are first made explicit. By substituting \code{"A+B*D"} in place
#'   of \code{C} in the second asf of the candidate and minimizing, one gets
#'   \code{"A+B*D<->G"}, which represents the indirect causal relevance, as
#'   claimed by the candidate, of \code{"A+B*D"} on \code{G}. This expression is
#'   then tested against the target as in the first phase: the target asf with
#'   \code{G} as the outcome is manipulated to reflect the indirect claims that
#'   the target makes about \code{G}. After substitution and minimization, we
#'   get \code{"A+D<->G"}, meaning that the target does \emph{not} make a claim
#'   of indirect causal relevance of \code{B} for \code{G}. That the candidate
#'   and the target are incompatible in their indirect causal claims is shown by
#'   the fact that \code{"A+B*D<->G"} is not a submodel of \code{"A+D<->G"}, and
#'   the function returns FALSE.
#'
#'   Due to the computational demands of some of the steps in the above
#'   procedure, \code{is_compatible} is an approximation, rather than strictly
#'   valid check for causal compatibility of \code{cna} models. Since the
#'   syntactic manipulations and, especially, minimization of the resulting
#'   expressions is so costly, \code{is_compatible} relies on the
#'   [`rreduce()`][cna::rreduce()] function from the [cna][cna::cna-package]
#'   package for minimization. [`rreduce()`][cna::rreduce] randomly chooses a
#'   single reduction path to produce only one minimal form of an expression
#'   whenever more than one exists, i.e. when the expression is ambiguous in its
#'   causal claims. In the case of ambiguous models, the output of
#'   \code{is_compatible} may depend on which reduction path(s) were chosen.
#'   These cases are rare enough to not significantly affect the intended use of
#'   \code{is_compatible} in the context of `frscore`. Another instance of
#'   \code{is_compatible} taking a shortcut is when processing cyclic models
#'   like \code{"(A+B<->C)*(C+D<->A)"}. Here the problems are as much
#'   philosophical as computational. It is clear that a cyclic candidate model
#'   cannot be compatible with a non-cyclic target. However, problems arise when
#'   trying to determine if a non-cyclic candidate is compatible with a cyclic
#'   target: it is not clear what counts as an incompatibility in causal
#'   ordering, given that a cyclic target model does not (causally) order its
#'   factors strictly, but a non-cyclic candidate does. Since many conclusions
#'   can be argued for here but some approach must be taken to ensure that
#'   \code{is_compatible()} works on all valid \code{cna} models,
#'   \code{is_compatible()} takes the least costly option and simply checks
#'   whether the candidate is a submodel of the target, and returns the result.
#'
#'
#' @importFrom cna C_is_submodel cyclic extract_asf full.ct getCond hstrsplit
#'   lhs
#' @importFrom cna noblanks relist1 rhs rreduce selectCases
#' @importFrom stats setNames
#' @export
is_compatible <- function(x, y, dat = NULL){
  x <- noblanks(x)
  y <- noblanks(y)
  out <- vector("logical", 1)
  # attributes(out) <- list(why = "",
  #                         x = x,
  #                         y = y,
  #                         ultimate_asfs = NULL,
  #                         cand_asfs_checked = NULL,
  #                         og_y = y,
  #                         og_x = x)
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

  # if(!is.inus(x, selectCases(y, x = dat))){
  #   out[1] <- FALSE
  #   attr(out, "why") <- "x is not inus wrt selectCases(y)"
  #   return(out)
  # }
  is_sm <- fsubmodel_csf(x,y)
  c_asfcount <- unlist(strsplit(x, "<->"))
  is_x_csf <- length(c_asfcount) > 2
  if(!is_x_csf & is_sm){
    out[1] <- TRUE
    #attr(out, "why") <- "x is asf and a submodel of y"
    return(out)
  }
  if(cyclic(x) || cyclic(y)){
    out[1] <- is_sm
    #attr(out, "why") <- "x or y is cyclic, only submodel relation between x and y tested"
    return(out)
  }
  dy <- decompose_model(y)
  out <- subin_target_ccomp(x =x, y = dy, ogy = y, dat = dat, type = type)
  if(!out){
    return(out)
  }
  #if(is_x_csf & is_sm){
  x <- is_comp_subtar(x, dat = dat, type = type)
     #attr(out, "why") <- "x is a csf and a submodel of y, substitute in x before checking compatibility"
  #}
  out <- subin_target_ccomp(x =x, y = dy, ogy = y, dat = dat, type = type)
  return(out)
}

subin_target_ccomp <- function(x, y, ogy, dat = NULL, type){
  prepared <- ccheck_prep(x, y, ogy)
  prep_target <- prepared$target_lhss
  #asf_subms <- fsubmodel_csf(prepared$candidate_asfs, y)
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
  #attr(correct, "target") <- NULL

  #out[1] <- all(correct)
  out <- all(correct)
  # if(out[1]){
  #   attr(out, "why") <- "all x asfs are submodels of expanded y asfs"
  # } else {
  #   attr(out, "why") <- "some x asfs are not submodels of expanded y asfs"
  # }
  #attr(out, "expanded_tar_asfs") <- subbed_tar_asfs
  #attr(out, "cand_asfs_checked") <- correct
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
  tar_outs_flipped <- sapply(tar_outs, case_flipper) #fix for mv
  cand_out <- names(x)
  outc_matches <- tar_outs %in% cand_out
  if(!any(outc_matches)){
    return(ogy)
  }
  outcome_match <- which(outc_matches)

  ultimate_lhs <- ultimate_lhs1 <- y[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))

  # while(any(sapply(toupper(tar_outs[!not_subbable]),
  #           function(x) grepl(x, toupper(ultimate_lhs))))){
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
  # sub_from_capmatch <- lapply(x$rhss,
  #                             function(y)
  #                               grepl(paste0("(?<![[:alpha:]])", y, "(?!\\w)"),
  #                                     x$lhss,
  #                                     perl = TRUE))
  sub_from_capmatch <- lapply(x$rhss,
                              function(y) fac_grepl(y, x$lhss))
  id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))

  # sub_from_capflip <- lapply(case_flipper(x$rhss),
  #                            function(y)
  #                              grepl(y, x$lhss)) #this needs fixing
  # sub_from_capflip <- lapply(case_flipper(x$rhss),
  #                            function(y)
  #                              grepl(paste0("(?<![[:alpha:]])",y, "(?!\\w)"),
  #                                    x$lhss, perl = TRUE)) #this needs fixing

  sub_from_capflip <- lapply(case_flipper(x$rhss),
                             function(y) fac_grepl(y, x$lhss))
  id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  while(any(c(id_sub_capflip, id_sub_capmatch))){
    for(i in seq_along(sub_from_capmatch)){
      if(id_sub_capmatch[i]){
        #x$lhss[sub_from_capmatch[[i]]] <- gsub(x$rhss[i],
        x$lhss[sub_from_capmatch[[i]]] <- gsub(paste0("(?<![[:alpha:]])",x$rhss[i],"(?!\\w)"),
                                               paste0("(", x$lhss[i], ")"),
                                               x$lhss[sub_from_capmatch[[i]]],
                                               perl = TRUE)
      }
    }
    for(i in seq_along(sub_from_capflip)){
      if(id_sub_capflip[i]){
        #x$lhss[sub_from_capflip[[i]]] <- gsub(tolower(x$rhss[i]),
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
    # sub_from_capmatch <- lapply(x$rhss,
    #                             function(y)
    #                               grepl(paste0("(?<![[:alpha:]])", y, "(?!\\w)"),
    #                                     x$lhss,
    #                                     perl = TRUE))
    sub_from_capmatch <- lapply(x$rhss,
                                function(y) fac_grepl(y, x$lhss))
    id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))

    # sub_from_capflip <- lapply(case_flipper(x$rhss),
    #                            function(y)
    #                              grepl(paste0("(?<![[:alpha:]])",y, "(?!\\w)"),
    #                                    x$lhss, perl = TRUE))
    sub_from_capflip <- lapply(case_flipper(x$rhss),
                               function(y) fac_grepl(y, x$lhss))
    id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  }
  x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
  return(x)
}


# chain_substituter <- function(x,
#                               subbed_from = vector("logical", length(x[[1]]))){
#   sub_from_capmatch <- lapply(x$rhss,
#                               function(y)
#                                 grepl(y, x$lhss))
#   id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
#
#   sub_from_capflip <- lapply(case_flipper(x$rhss),
#                              function(y)
#                                grepl(y, x$lhss))
#   id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
#   if(!any(c(id_sub_capflip, id_sub_capmatch))){
#     x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
#     return(x)
#   }
#   for(i in seq_along(sub_from_capmatch)){
#     if(id_sub_capmatch[i]){
#       x$lhss[sub_from_capmatch[[i]]] <- gsub(x$rhss[i],
#                                               paste0("(", x$lhss[i], ")"),
#                                               x$lhss[sub_from_capmatch[[i]]])
#     }
#   }
#   for(i in seq_along(sub_from_capflip)){
#     if(id_sub_capflip[i]){
#       x$lhss[sub_from_capflip[[i]]] <- gsub(tolower(x$rhss[i]),
#                                              paste0("!(", x$lhss[i], ")"),
#                                              x$lhss[sub_from_capflip[[i]]])
#     }
#   }
#   wh_subbed_from <- which(subbed_from)
#   wh_capmatch <- which(id_sub_capmatch)
#   wh_capflip <- which(id_sub_capflip)
#   w_subbed <- unique(c(wh_subbed_from, wh_capmatch, wh_capflip))
#   subbed_from[w_subbed] <- TRUE
#   chain_substituter(x, subbed_from = subbed_from)
# }

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

















