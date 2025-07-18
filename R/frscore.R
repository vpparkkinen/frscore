#' frscore
#'
#' @description Calculate fit-robustness scores for a set of \code{cna}
#'   solutions
#'
#' @param sols Character vector that contains the solutions to be scored.
#' @param normalize Character vector that determines the method used in
#'   normalizing the scores. "truemax" normalizes by the actual highest score
#'   such that the highest scoring solution types get score 1. "idealmax"
#'   normalizes by a theoretical maximum score calculated by assuming that
#'   all solutions of equal complexity are identical, and for every solution
#'   of a given complexity, all solutions with lower complexity are its
#' submodels.
#' @param maxsols Integer determining the maximum number of unique solution
#'   types found in \code{sols} to be included in the scoring.
#' @param verbose Logical; if \code{TRUE}, calculate and print additional
#'   information about submodel relations among the unique solutions types found
#'   in \code{sols}. Defaults to \code{FALSE}, which makes execution slightly
#'   faster.
#' @param print.all Logical, controls the number of entries printed when
#'   printing the results. If \code{TRUE}, results are printed as when using
#'   \code{print.data.frame} defaults. If \code{FALSE}, 20 highest scoring
#'   models are printed.
#'
#' @details \code{frscore} is intended to be used for calculating the
#'   fit-robustness scores of \code{cna} solutions obtained by reanalyzing
#'   a data set repeatedly, using different consistency and coverage
#'   thresholds in each analysis.
#'
#' @returns A named list where the first element is a data frame containing
#'   the unique solution types and their scores. Rest of the elements
#'   contain additional information about the submodel relations among
#'   the unique solutions types (if \code{verbose = TRUE}) and how
#'   the function was called.
#'
#' @examples
#'   models <- replicate(20, cna::randomCsf(4, outcome = "A"))
#'   results <- frscore(sols = models, verbose = TRUE)
#'   results
#'



#' @importFrom Rfast floyd
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
frscore <- function(sols,
                    dat = NULL,
                    normalize = c("truemax", "idealmax", "none"),
                    maxsols = 50,
                    verbose = FALSE,
                    print.all = FALSE,
                    comp.method = c("causal_submodel", "is.submodel")
){
  withr::local_collate("C")
  call <- match.call()
  if (length(sols) == 0){
    warning('no solutions to test')
    return(NULL)
  }
  if (NA %in% sols){
    sols <- sols[!is.na(sols)]
  }

  if (inherits(sols, c("stdAtomic", "stdComplex"))){
    sols <- as.character(sols)
  } else {
    sols <- as.character(sols)
    sols <- stxstd(sols)
  }
  normalize <- match.arg(normalize)
  compmeth <- match.arg(comp.method)
  compscoring <- switch(compmeth, causal_submodel = TRUE, is.submodel = FALSE)
  sols <- sols[order(sols)]

  mf <- as.data.frame(table(sols), stringsAsFactors = FALSE)
  mf$cx <- cna::getComplexity(mf[,1])
  excluded_sols <- if (nrow(mf) < maxsols) 0 else nrow(mf) - maxsols
  cat("processing", nrow(mf), "unique model types,\nmaxsols set to",
      paste0(maxsols,","), "excluding",
      if (nrow(mf) > maxsols) (nrow(mf)-maxsols) else 0,
      "model types from scoring\n\n")
    if (nrow(mf) == 1){
      sco <- (mf$Freq-1)*2
      out <- data.frame(model = mf$sols,
                      score = sco,
                      tokens = mf$Freq,
                      stringsAsFactors = FALSE)
      elems <- mf[,c(1,2)]
      elems$Freq <- (elems$Freq-1)*2
      colnames(elems) <- c("model", "score")
      scsums <- list(elems)
      names(scsums) <- sols[1]
      tmat <- matrix(nrow = nrow(mf),
                     ncol = nrow(mf),
                     dimnames = list(mf[,1], mf[,1]))
  } else if (length(unique(mf$cx)) == 1){
      sco <- (mf$Freq-1)*2
      out <- data.frame(model = mf$sols,
                      score = sco,
                      tokens = mf$Freq,
                      stringsAsFactors = FALSE)

      elems <- mf[,c(1,2)]
      elems$Freq <- (elems$Freq-1)*2
      colnames(elems) <- c("model", "score")
      scsums <- split(elems, elems$model)
      scsums <- lapply(scsums,
                     function(x) {if(x$score == 0){
                       x <- data.frame(model = character(),
                                       score = numeric())} else {
                                         x <- x
                                       };return(x)})
    names(scsums) <- unique(sols)
    zeroid <- unlist(lapply(scsums, function(x) x[[1]] < 1))
    scsums[zeroid] <- list(NULL)
    maxsols <- "ignored"
    tmat <- matrix(nrow = nrow(mf), ncol = nrow(mf), dimnames = list(mf[,1], mf[,1]))
  } else {
    t_score <- tmat_scoring(mf, maxsols, compscoring, dat)
    out <- t_score[[1]]
    scsums <- t_score[[2]]
    tmat <- t_score[[3]]
    }

  if(normalize == "truemax"){
    if (max(out$score>=1)){out$norm.score <- out$score / max(out$score)} else
    {out$norm.score <- 0L}
  }

  if(normalize == "idealmax"){
    compx <- rep(mf$cx, mf$Freq)
    cfreqtab <- as.data.frame(table(compx), stringsAsFactors = FALSE)
    cfreqtab$compx <- as.integer(as.character(cfreqtab$compx))
    cfreqtab <- cfreqtab[order(cfreqtab$compx, decreasing = T),]
    cfreqtab$selfscore <- (cfreqtab$Freq - 1) * 2
    otherscore <- vector("integer", nrow(cfreqtab))
      for (i in seq_along(1:nrow(cfreqtab))) {
        otherscore[i] <- sum(cfreqtab[-i,]$Freq)
      }

    cfreqtab$otherscore <- otherscore
    idealmaxscore <- max(cfreqtab$selfscore + cfreqtab$otherscore)
    if (max(out$score>=1)){out$norm.score <- out$score / idealmaxscore} else {
      out$norm.score <- 0L
    }

  }
  out <- out[order(out$score, decreasing = T),]
  # relscore <- (out$score - (out$tokens - 1)) / (sum(out$tokens) - 1)
  # out$rel.score <- if (anyNA(relscore)) 0L else relscore
  rownames(out) <- 1:nrow(out)

  scsums <- scsums[sapply(out$model, function(x) which(x == names(scsums)))]

  return(structure(list(models = out,
                        verbose = verbose,
                        verbout = scsums,
                        print.all = print.all,
                        normal = normalize,
                        maxsols = list(maxsols = maxsols, excluded = excluded_sols),
                        comp.method = comp.method,
                        submodel_adjacencies = if(exists("tmat", inherits = FALSE)){
                          tmat
                        } else {NULL},
                        call = call
  ), class = "frscore"))

}

tmat_scoring <- function(mf, maxsols, compscoring, dat){
  compsplit <- split(mf, mf$cx)
  if (nrow(mf) > maxsols){
    compsplit <- lapply(compsplit, function(x) x[order(x[,2], decreasing = T),])
    ngroups <- length(compsplit)
    if (ngroups == 1){mf <- mf[1:maxsols, ]} else {
      sizes <- sapply(compsplit, nrow)
      n_pick <- as.integer((maxsols / ngroups) + 1)
      picks <- vector("integer", ngroups)
      picks[1] <- n_pick
      for (i in 2:length(sizes)){
        r <- ifelse(sizes[i-1] > picks[i-1], 0, picks[i-1] - sizes[i-1])
        picks[i] <- r + n_pick
      }
      chosen <- vector("list", ngroups)
      for (i in seq_along(chosen)){
        nr <- ifelse(nrow(compsplit[[i]]) < picks[i],
                     nrow(compsplit[[i]]), picks[i])
        chosen[[i]] <- compsplit[[i]][1:nr,]
      }
      mf <- as.data.frame(do.call(rbind, chosen), stringsAsFactors = FALSE)
      if (nrow(mf) > maxsols){
        mf <- mf[1:maxsols, ]}
    }
    mf <- mf[order(mf[,3], decreasing = T),]
    compsplit <- split(mf, mf$cx)
  }

  if (length(compsplit) == 1L){
    mf <- compsplit[[1]]
    sco <- (mf$Freq-1)*2

    out <- data.frame(model = mf$sols,
                      score = sco,
                      tokens = mf$Freq,
                      stringsAsFactors = FALSE)

    elems <- mf[,c(1,2)]
    elems$Freq <- (elems$Freq-1)*2
    colnames(elems) <- c("model", "score")
    scsums <- split(elems, elems$model)
    scsums <- lapply(scsums,
                     function(x) {if(x$score == 0){
                       x <- data.frame(model = character(),
                                       score = numeric())} else {
                                         x <- x
                                       };return(x)})
    names(scsums) <- unique(mf$sols)
    zeroid <- unlist(lapply(scsums, function(x) x[[1]] < 1))
    scsums[zeroid] <- list(NULL)
    tmat <- matrix(nrow = nrow(mf), ncol = nrow(mf), dimnames = list(mf[,1], mf[,1]))
  } else {
    mf <- mf[order(mf[,3], decreasing = T),]
    sscore <- vector("list", length(compsplit)-1)
    tmat <- matrix(nrow = nrow(mf), ncol = nrow(mf), dimnames = list(mf[,1], mf[,1]))
    nmod <- nrow(tmat)
    tot_sc <- (nrow(mf)^2)-nrow(mf)
    tot_sc <- ifelse(nrow(mf) > maxsols, (maxsols^2)-maxsols, tot_sc)
    cat("0 /", tot_sc , "submodel relations tested\r")
    cspl_ch <- 0
    for (m in 1:(length(compsplit)-1)){
      subres <- sapply(1:nrow(compsplit[[m]]), function(p)
        lapply(1:nrow(compsplit[[m+1]]),
               function(x)
                 if(compscoring){
                   comptest(compsplit[[m]][p,1],
                            compsplit[[m+1]][x,1],
                            dat = dat)
                 } else {
                   subAdd(compsplit[[m]][p,1], compsplit[[m+1]][x,1])
                 }))
      sscore[[m]] <- do.call(rbind, subres)
      cspl_ch <- cspl_ch + nrow(sscore[[m]])
      cat(cspl_ch,
          "/", tot_sc, "potential submodel relations tested\r")
    }
    scs <- do.call(rbind, sscore)

    for (i in 1:nrow(tmat)){
      tmat[i,i] <- NA
    }
    tmat_b <- tmat
    for (i in 1:nrow(scs)){
      tmat[which(rownames(tmat) == scs[i,1]), which(colnames(tmat) == scs[i,2])] <- scs[i,3]
      tmat_b[which(rownames(tmat_b) == scs[i,1]), which(colnames(tmat_b) == scs[i,2])] <- scs[i,4]
    }

    for(c in seq_along(compsplit)){
      tmat_b[which(rownames(tmat_b) %in% compsplit[[c]]$sols),
             which(colnames(tmat_b) %in% compsplit[[c]]$sols)] <- 1
      cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
          tot_sc, "potential submodel relations tested \r")
    }

    for(x in 1:(ncol(tmat_b)-1)){
      tmat_b[x, (x+1):ncol(tmat_b)] <- 1
      cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
          tot_sc, "potential submodel relations tested \r")
    }
    subm_paths <- floyd(tmat)
    s_closures <- !apply(subm_paths, 2, is.na)
    tmat_b[s_closures] <- tmat[s_closures] <- 1
    cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
        tot_sc, "potential submodel relations tested \r")
    nci <- apply(tmat_b, 2, is.na)
    if(any(is.na(tmat_b))){
      tmat <- t(tmat)
      tmat_b <- t(tmat_b)
      while(anyNA(tmat_b)){
        nas <- which(apply(tmat_b, 2, is.na))
        nacol_rles <- rle(col(tmat)[nas])
        ids <- nas[1:nacol_rles$lengths[1]]
        chks <- lapply(ids, function(x)
          if(compscoring){comptest(colnames(tmat)[col(tmat)[x]],
                                   rownames(tmat)[row(tmat)[x]], dat = dat)} else {
                                     subAdd(colnames(tmat)[col(tmat)[x]],
                                            rownames(tmat)[row(tmat)[x]])
                                   })

        for(n in seq_along(chks)){
          tmat[ids[n]] <- chks[[n]][,3]
          tmat_b[ids[n]] <- chks[[n]][,4]
          cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
              tot_sc, "potential submodel relations tested \r")
        }

        ress <- unlist(sapply(chks, "[", 3))

        if (any(!is.na(ress))){
          subm_paths <- floyd(tmat)
          s_closures <- !apply(subm_paths, 2, is.na)
          tmat_b[s_closures] <- tmat[s_closures] <- 1
          cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
              tot_sc, "potential submodel relations tested \r")
        }
        cat(sum(tmat_b, na.rm = TRUE) - nmod, "/",
            tot_sc, "potential submodel relations tested \r")
      }
    }
    cat(tot_sc, "/", tot_sc, "potential submodel relations tested \n\n")

    for (i in 1:nrow(tmat)){
      tmat[i,i] <- NA
    }

    hits <- apply(tmat, 2, function(x) x == 1)
    nohits <- apply(tmat, 2, is.na)

    if(all(is.na(hits))){prescore <- data.frame(mod = character(),
                                                subsc = integer(),
                                                supmod = character(),
                                                supsc = integer())
    } else {
      prescore <- data.frame(mod = rownames(tmat)[row(tmat)[which(hits)]],
                             subsc = 0,
                             supmod = colnames(tmat)[col(tmat)[which(hits)]],
                             supsc = 0, stringsAsFactors = FALSE)
      prescore <- prescore %>% dplyr::filter(.data$mod != .data$supmod)
      prescore <- prescore %>% dplyr::left_join(mf[,1:2], by = c("mod" = "sols")) %>%
        dplyr::mutate(supsc = .data$Freq) %>% dplyr::select(-"Freq")
      prescore <- prescore %>% dplyr::left_join(mf[,1:2], by = c("supmod" = "sols")) %>%
        dplyr::mutate(subsc = .data$Freq) %>% dplyr::select(-"Freq")
    }
    prescore_neg <- data.frame(mod = rownames(tmat)[row(tmat)[which(nohits)]],
                               subsc = 0,
                               supmod = colnames(tmat)[col(tmat)[which(nohits)]],
                               supsc = 0, stringsAsFactors = FALSE)
    sc <- rbind(prescore, prescore_neg)
    mf <- mf[order(mf$sols),]

    #scsums <- verbosify(sc, mf, scoretype)
    scsums <- verbosify(sc, mf)
    pre.ssc <- sc[,c(1,2)] %>% dplyr::group_by(.data$mod) %>%
      dplyr::mutate(subsc = sum(.data$subsc)) %>%
      dplyr::distinct()
    pre.susc <- sc[,c(3,4)] %>% dplyr::group_by(.data$supmod) %>%
      dplyr::mutate(supsc = sum(.data$supsc)) %>% dplyr::distinct()
    pre.ssc <- pre.ssc[order(pre.ssc$mod),]
    pre.susc <- pre.susc[order(pre.susc$supmod),]
    preout <- pre.ssc$subsc + pre.susc$supsc + (mf$Freq-1)*2
    out <- data.frame(model = mf$sols,
                      score = preout,
                      tokens = mf$Freq,
                      stringsAsFactors = FALSE)
    tmat[is.na(tmat)] <- 0L
  }
  return(list(out, scsums, tmat))
}




#' @importFrom cna is.submodel
subAdd <- function(x, y){
  re <- is.submodel(x,y)
  return(data.frame(x,
                    y,
                    ifelse(re[[1]] == TRUE, 1, NA),
                    checked = 1,
                    stringsAsFactors = FALSE))
}

comptest <- function(x, y, dat = NULL){
  re <- causal_submodel(x, y, dat = dat)
  return(data.frame(x,
                    y,
                    ifelse(re[[1]] == TRUE, 1, NA),
                    checked = 1,
                    stringsAsFactors = FALSE))
}

verbosify <- function(sc, mf){
  bs <- sc[, c(1,3,4)]
  colnames(bs)[colnames(bs) == "supsc"] <- "sub.frequency"
  colnames(bs)[colnames(bs) == "mod"] <- "model"
  bysup <- bs %>% dplyr::group_split(.data$supmod)
  supnames <- unlist(lapply(bysup, function(x) unique(x$supmod)))
  names(bysup) <- supnames
  subspermod <- lapply(bysup, function(x) x[,c(1,3)])
  subspermod <- lapply(subspermod,
                       function(x) as.data.frame(x, stringsAsFactors = FALSE))
  subspermod <- lapply(subspermod, function(x) x[order(x$model),])

  sps <- sc[, c(1,2,3)]
  sps <- data.frame(supermodel = sps[,3], sup.frequency = sps[,2],
                    mod = sps[,1],
                    stringsAsFactors = FALSE)
  bysub <- sps %>% dplyr::group_split(.data$mod)
  subnames <- supnames <- unlist(lapply(bysub, function(x) unique(x$mod)))
  names(bysub) <- subnames
  bysub <- lapply(bysub, function(x) x[order(x$supermodel),])
  superpermod <- lapply(bysub, function(x) x[,2])
  superpermod <- lapply(superpermod,
                        function(x) as.data.frame(x, stringsAsFactors = FALSE))

  robbasis <- mapply(cbind, subspermod, superpermod, SIMPLIFY = F)
  mfs <- mf[,c(1,2)]
  colnames(mfs)[colnames(mfs) == "sols"] <- "model"
  dups <- lapply(names(robbasis), function(x) mfs[mfs[,1]==x,])
  dupscores <- lapply(dups, function(x) x %>%
                        dplyr::mutate(sub.frequency=.data$Freq-1,
                                      sup.frequency = .data$Freq-1,
                                      Freq = NULL))
  dupscores <- lapply(dupscores, function(x) if(x[,2] == 0){x[-1,]}else{x})
  robbasis <- mapply(rbind, robbasis, dupscores, SIMPLIFY = F)
  robred <- lapply(robbasis, function(x) x[x[,2] + x[,3] > 0,])


  scsums <- lapply(robred, function(x){
    if(nrow(x) == 0){
      x<-NULL
    } else {
        x$score <- apply(x[,c(2,3)], 1, sum);return(x[,c(1,4)])
        }
    })
  scsums <- lapply(scsums, function(x) x[x[,2] > 0,])
  return(scsums)
}


#' @importFrom rlang abort
stxstd <- function(sols){
  mods <- cna::noblanks(sols)
  asfs <- cna::extract_asf(mods)
  cspattern <- "^([A-Za-z]+[A-Za-z0-9]*)(\\*([A-Za-z]+[A-Za-z0-9]*)+)*(\\+([A-Za-z]+[A-Za-z0-9]*)(\\*([A-Za-z]+[A-Za-z0-9]*))*)*(->|<->)([A-Za-z]+[A-Za-z0-9]*)$"
  mvpattern <- "^([A-Z]+[A-Za-z0-9]*=[0-9]+)(\\*([A-Z]+[A-Za-z0-9]*=[0-9]+)+)*(\\+([A-Z]+[A-Za-z0-9]*=[0-9]+)(\\*([A-Z]+[A-Za-z0-9]*=[0-9]+))*)*(<->|->)([A-Z]+[A-Za-z0-9]*=[0-9]+)$"
  maybemv <- grepl("=[0-9]+", mods)
  allmv <- all(maybemv)
  if(any(maybemv) & !allmv){
    abort("Inconsistent model types: `sols` appears to include both multi-valued
         and binary models")
  }
  if(allmv){
    pattern <- mvpattern
  } else {
    pattern <- cspattern
  }
  notok <- lapply(asfs, function(x) any(!grepl(pattern, x)))
  notok <- unlist(notok)

  if (any(notok)){
    abort(paste0("Invalid model syntax: ", sols[notok]))
  }
  ocs <- lapply(asfs, cna::rhs)
  ocsordered <- lapply(ocs, order)
  dnfs <- lapply(asfs, cna::lhs)
  dnfs <- mapply(function(x, y) x[y], dnfs, ocsordered, SIMPLIFY = F)
  dnfs <- lapply(dnfs, cna::stdCond)
  ocs <- mapply(function(x, y) x[y], ocs, ocsordered, SIMPLIFY = F)
  preasf <- mapply(function(x, y) mapply(function(p, q){
    if(grepl("<->", p)){paste0(q, "<->")
    } else {
        paste0(q, "->")
      }
    }, x, y), asfs, dnfs, SIMPLIFY = F)
  stdasfs <- mapply(function(x, y) paste0(x, y), preasf, ocs, SIMPLIFY = F)
  out <- lapply(stdasfs, function(x) {if(length(x) > 1){
    x <- paste0(paste0("(", x, ")"), collapse = "*")
    } else {x <- x}; return(x)
  })
  out <- unlist(out)
  return(out)
}



# Print method for frscore()
#' @export
#' @importFrom utils head
print.frscore <- function(x,
                          verbose = x$verbose,
                          verbout = x$verbout,
                          print.all = x$print.all,
                          maxsols = x$maxsols,
                          ...){
  #cat("FRscore, score type: full", "||", "score normalization:", x$normal, "\n\n")
  cat("FRscore, score normalization:", x$normal, "\n\n")
  if(maxsols$maxsols == "ignored"){
    cat("no submodel checks were needed, argument 'maxsols' ignored \n")
  } else {
    cat("maxsols set to", maxsols$maxsols, "--", maxsols$excluded, "solution types excluded from scoring \n\n")
  }
  cat("-----\n \n")
  cat("Model types: \n")
  cat("\n")
  if(print.all){
    print(x$models, n = Inf)
  } else {
    print(head(x$models, n = 20L))
    cat("\n")
    nr <- nrow(x$models) - 20L
    if(nr > 0){cat('...there were', nr, 'more model types, use \'print.all = TRUE\' to print all \n')}
    cat('\n')

  }

  if(verbose & !is.null(verbose)){
    cat('\n')
    cat('Score composition: \n')
    cat('----- \n \n')
    print(verbout)
    invisible(x)
  } else {invisible(x)}
}

