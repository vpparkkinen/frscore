#' frscore
#'
#' @description Calculate fit-robustness scores for a set of \code{cna}
#'   solutions
#'
#' @param sols Character vector that contains the solutions to be scored.
#' @param scoretype Character vector specifying the scoring method: "full"
#'   (default, scoring is based on counting sub- and supermodels), "supermodel"
#'   (count supermodels) "submodel" (count submodels).
#' @param normalize Character vector that determines the method used in
#'   normalizing the scores. "truemax" normalizes by the actual highest score
#'   such that the highest scoring solution types get score 1. "idealmax"
#'   normalizes by a theoretical maximum score calculated by assuming that
#'   all solutions of equal complexity are identical, and for every solution
#'   of a given complexity, all solutions with lower complexity are its
#'   submodels.
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


#' @import cna
#' @importFrom Rfast floyd
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
frscore <- function(sols,
                    #normalize = TRUE,
                    scoretype = c("full", "supermodel", "submodel"),
                    normalize = c("truemax", "idealmax", "none"),
                    maxsols = 50,
                    verbose = FALSE,
                    print.all = FALSE){
  if (typeof(sols) != "character"){
    stop("sols should be a character vector of CNA solutions, not object of type ", typeof(sols))
  }
  if (length(sols) == 0){
    warning('no solutions to test')
    return(NULL)
  }
  if (NA %in% sols){
    sols <- sols[!is.na(sols)]
  }

  scoretype <- match.arg(scoretype)
  normalize <- match.arg(normalize)
  sols <- sols[order(sols)]
  mf <- as.data.frame(table(sols), stringsAsFactors = FALSE)
  mf$cx <- cna:::getComplexity(mf[,1])
  excluded_sols <- 0

  if(length(sols) == 1){
    out <- data.frame(model = sols, score = 0L, stringsAsFactors = FALSE)
    scsums <- list(NULL)
    names(scsums) <- sols
  } else if (nrow(mf) == 1){
    if(scoretype %in% c("submodel", "supermodel")){
      sco <- (mf$Freq-1)*2/2
    } else {
      sco <- (mf$Freq-1)*2
    }

    out <- data.frame(model = mf$sols, score = sco, tokens = mf$Freq, stringsAsFactors = FALSE)
    #if(normalize){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}
    elems <- (mf$Freq-1)*2
    if (scoretype %in% c("supermodel", "submodel")){elems <- elems / 2}
    names(elems) <- sols[1]
    scsums <- list(elems)
    names(scsums) <- sols[1]


  } else if (length(unique(mf$cx)) == 1){
    if(scoretype %in% c("submodel", "supermodel")){
      sco <- (mf$Freq-1)*2/2
    } else {
      sco <- (mf$Freq-1)*2
    }

    out <- data.frame(model = mf$sols, score = sco, tokens = mf$Freq, stringsAsFactors = FALSE)
    scsums <- (mf$Freq-1)*2
    if (scoretype %in% c("supermodel", "submodel")){scsums <- scsums / 2}
    scsums <- as.list(scsums)
    for (i in seq_along(scsums)){names(scsums[[i]]) <- unique(sols)[i]}

    names(scsums) <- unique(sols)
    zeroid <- unlist(lapply(scsums, function(x) x[[1]] < 1))
    scsums[zeroid] <- list(NULL)

    #cat("no submodel tests were required, argument 'maxsols' is ignored \n\n")
    maxsols <- "ignored"
  } else {

    compsplit <- mf %>% dplyr::group_split(.data$cx)
    #compsplit <- lapply(compsplit, function(x) x[order(x[,3], decreasing = T),])


    if (nrow(mf) > maxsols){
      excluded_sols <- nrow(mf) - maxsols
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
        mf <- as.data.frame(do.call(rbind, chosen))
        if (nrow(mf) > maxsols){
          mf <- mf[1:maxsols, ]}
      }

      mf <- mf[order(mf[,3], decreasing = T),]
      compsplit <- mf %>% dplyr::group_split(.data$cx)
    }

    mf <- mf[order(mf[,3], decreasing = T),]

    sscore <- vector("list", length(compsplit)-1)
    tmat <- matrix(nrow = nrow(mf), ncol = nrow(mf), dimnames = list(mf[,1], mf[,1]))


    for (m in 1:(length(compsplit)-1)){

      subres <- sapply(1:nrow(compsplit[[m]]), function(p)
        lapply(1:nrow(compsplit[[m+1]]),
               function(x) subAdd(compsplit[[m]][p,1], compsplit[[m+1]][x,1])))
      sscore[[m]] <- do.call(rbind, subres)
    }
    scs <- do.call(rbind, sscore)


    for (i in 1:nrow(tmat)){tmat[i,i] <- NA}

    tmat_b <- tmat

    for(x in 1:(ncol(tmat_b)-1)){
      tmat_b[x, (x+1):ncol(tmat_b)] <- 1
    }

    for(c in seq_along(compsplit)){
      tmat_b[which(rownames(tmat_b) %in% compsplit[[c]]$sols),
             which(colnames(tmat_b) %in% compsplit[[c]]$sols)] <- 1
    }

    for (i in 1:nrow(scs)){
      tmat[which(rownames(tmat) == scs[i,1]), which(colnames(tmat) == scs[i,2])] <- scs[i,3]
      tmat_b[which(rownames(tmat_b) == scs[i,1]), which(colnames(tmat_b) == scs[i,2])] <- scs[i,4]
    }

    subm_paths <- floyd(tmat)
    s_closures <- !apply(subm_paths, 2, is.na)
    tmat_b[s_closures] <- tmat[s_closures] <- 1
    nci <- apply(tmat_b, 2, is.na)

    if(any(is.na(tmat_b))){
      # not_chk <- data.frame(sub = rownames(tmat)[row(tmat)[which(nci)]],
      #                       super = colnames(tmat)[col(tmat)[which(nci)]])
      #
      # not_chk$cxs <- cna:::getComplexity(not_chk$sub)
      # not_chk <- not_chk[order(not_chk$cxs, decreasing = T),]
      #
      tmat <- t(tmat)
      tmat_b <- t(tmat_b)
      while(anyNA(tmat_b)){
        #test <- subAdd(not_chk[counter, 1], not_chk[counter, 2])
        # id <- min(which(is.na(tmat_b)))
        # test <- subAdd(rownames(tmat)[row(tmat)[id]],
        #                colnames(tmat)[col(tmat)[id]])
        #
        #
        # tmat[which(rownames(tmat) == test[,1]), which(colnames(tmat) == test[,2])] <- test[,3]
        # tmat_b[which(rownames(tmat_b) == test[,1]), which(colnames(tmat_b) == test[,2])] <- test[,4]
        # subm_paths <- floyd(tmat)
        # s_closures <- !apply(subm_paths, 2, is.na)
        # tmat_b[s_closures] <- tmat[s_closures] <- 1

        #narows <- which(apply(tmat_b, 2, is.na))
        #nacols <- col(tmat)[which(apply(tmat_b, 2, is.na))]

        nas <- which(apply(tmat_b, 2, is.na))
        nacol_rles <- rle(col(tmat)[nas])
        #nacols <- col(tmat)[nas]
        #  allids <- lapply(nacols, function(x) tmat[,x])


        # ids <- mapply(function(a,b) nas[a:b],
        #              (css - nacol_rles$lengths) +1, css, SIMPLIFY = F)
        #

        ids <- nas[1:nacol_rles$lengths[1]]

        chks <- lapply(ids, function(x)
          subAdd(colnames(tmat)[col(tmat)[x]],
                 rownames(tmat)[row(tmat)[x]]))
        for(n in seq_along(chks)){
          tmat[ids[n]] <- chks[[n]][,3]
          tmat_b[ids[n]] <- chks[[n]][,4]
        }

        ress <- unlist(sapply(chks, "[", 3))



        # id <- min(which(is.na(tmat_b)))
        # test <- subAdd(colnames(tmat)[col(tmat)[id]],
        #                rownames(tmat)[row(tmat)[id]])
        # tmat[id] <- test[,3]
        # tmat_b[id] <- test[,4]
        #
        # cumsum(nacol_rles$lengths)
        ###########
        # id <- min(which(is.na(tmat_b)))
        # test <- subAdd(colnames(tmat)[col(tmat)[id]],
        #                rownames(tmat)[row(tmat)[id]])
        #
        #
        # #tmat[which(rownames(tmat) == test[,1]), which(colnames(tmat) == test[,2])] <- test[,3]
        # #tmat_b[which(colnames(tmat_b) == test[,1]), which(rownames(tmat_b) == test[,2])] <- test[,4]
        # tmat[id] <- test[,3]
        # tmat_b[id] <- test[,4]
        # if (!is.na(test[,3])){
        #   subm_paths <- floyd(tmat)
        #   s_closures <- !apply(subm_paths, 2, is.na)
        #   tmat_b[s_closures] <- tmat[s_closures] <- 1
        # ###########
        if (any(!is.na(ress))){
          subm_paths <- floyd(tmat)
          s_closures <- !apply(subm_paths, 2, is.na)
          tmat_b[s_closures] <- tmat[s_closures] <- 1

        }
      }
    }

    for (i in 1:nrow(tmat)){tmat[i,i] <- NA}

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
                             supsc = 0)

      prescore <- prescore %>% dplyr::filter(.data$mod != .data$supmod)
      prescore <- prescore %>% dplyr::left_join(mf[,1:2], by = c("mod" = "sols")) %>%
        dplyr::mutate(supsc = .data$Freq) %>% dplyr::select(-.data$Freq)
      prescore <- prescore %>% dplyr::left_join(mf[,1:2], by = c("supmod" = "sols")) %>%
        dplyr::mutate(subsc = .data$Freq) %>% dplyr::select(-.data$Freq)
    }

    prescore_neg <- data.frame(mod = rownames(tmat)[row(tmat)[which(nohits)]],
                               subsc = 0,
                               supmod = colnames(tmat)[col(tmat)[which(nohits)]],
                               supsc = 0)

    sc <- rbind(prescore, prescore_neg)
    mf <- mf[order(mf$sols),]
    if(verbose){
      bs <- sc[, c(1,3,4)]
      colnames(bs)[colnames(bs) == "supsc"] <- "sub.frequency"
      colnames(bs)[colnames(bs) == "mod"] <- "model"
      bysup <- bs %>% dplyr::group_split(.data$supmod)
      supnames <- unlist(lapply(bysup, function(x) unique(x$supmod)))
      names(bysup) <- supnames
      subspermod <- lapply(bysup, function(x) x[,c(1,3)])
      subspermod <- lapply(subspermod, function(x) as.data.frame(x, stringsAsFactors = FALSE))
      subspermod <- lapply(subspermod, function(x) x[order(x$model),])

      sps <- sc[, c(1,2,3)]
      sps <- data.frame(supermodel = sps[,3], sup.frequency = sps[,2], mod = sps[,1])
      bysub <- sps %>% dplyr::group_split(.data$mod)
      subnames <- supnames <- unlist(lapply(bysub, function(x) unique(x$mod)))
      names(bysub) <- subnames
      bysub <- lapply(bysub, function(x) x[order(x$supermodel),])
      superpermod <- lapply(bysub, function(x) x[,2])
      superpermod <- lapply(superpermod, function(x) as.data.frame(x, stringsAsFactors = FALSE))

      robbasis <- mapply(cbind, subspermod, superpermod, SIMPLIFY = F)
      mfs <- mf[,c(1,2)]
      colnames(mfs)[colnames(mfs) == "sols"] <- "model"
      dups <- lapply(names(robbasis), function(x) mfs[mfs[,1]==x,])
      dupscores <- lapply(dups, function(x) x %>%
                            dplyr::mutate(sub.frequency=.data$Freq-1, sup.frequency = .data$Freq-1, Freq = NULL))
      dupscores <- lapply(dupscores, function(x) if(x[,2] == 0){x[-1,]}else{x})
      robbasis <- mapply(rbind, robbasis, dupscores, SIMPLIFY = F)
      robred <- lapply(robbasis, function(x) x[x[,2] + x[,3] > 0,])

      if (scoretype == "full") {
        scsums <- lapply(robred, function(x)
          if(nrow(x) == 0){x<-NULL}else{apply(x[,c(2,3)], 1, sum)})
      }
      if (scoretype == "supermodel") {
        scsums <- lapply(robred, function(x)
          if(nrow(x) == 0){x<-NULL}else{x[,3]})
      }
      if (scoretype == "submodel") {
        scsums <- lapply(robred, function(x)
          if(nrow(x) == 0){x<-NULL}else{x[,2]})
      }
      for (i in 1:length(scsums)){
        if(!is.null(scsums[[i]])){names(scsums[[i]]) <- robred[[i]][,1]}
      }

      scsums <- lapply(scsums, function(x) x[x>0])
      scsums <- lapply(scsums, function(x) if (length(x)<1){NULL}else{x})
    }

    pre.ssc <- sc[,c(1,2)] %>% dplyr::group_by(.data$mod) %>%
      dplyr::mutate(subsc = sum(.data$subsc)) %>%
      dplyr::distinct()
    pre.susc <- sc[,c(3,4)] %>% dplyr::group_by(.data$supmod) %>%
      dplyr::mutate(supsc = sum(.data$supsc)) %>% dplyr::distinct()
    pre.ssc <- pre.ssc[order(pre.ssc$mod),]
    pre.susc <- pre.susc[order(pre.susc$supmod),]
    # mf <- mf[order(mf$sols),]
    # if (scoretype == "full") {preout <- rep(pre.ssc$subsc, mf$Freq) + rep(pre.susc$supsc, mf$Freq) +
    #   (rep(mf$Freq, mf$Freq)-1)*2}
    #
    if (scoretype == "full") {preout <- pre.ssc$subsc + pre.susc$supsc +
      (mf$Freq-1)*2}

    # if (scoretype == "supermodel") {preout <- rep(pre.ssc$subsc, mf$Freq) +
    #   (rep(mf$Freq, mf$Freq)-1)*2/2}
    #
    if (scoretype == "supermodel") {preout <- pre.ssc$subsc +
      (mf$Freq-1)*2/2}


    # if (scoretype == "submodel") {preout <- rep(pre.susc$supsc, mf$Freq) +
    #   (rep(mf$Freq, mf$Freq)-1)*2/2}
    #
    if (scoretype == "submodel") {preout <- pre.susc$supsc +
      (mf$Freq-1)*2/2}


    #out <- data.frame(model = sols[1:length(preout)], score = preout, stringsAsFactors = FALSE)
    out <- data.frame(model = mf$sols, score = preout, tokens = mf$Freq, stringsAsFactors = FALSE)
    #out <- out %>% dplyr::group_by(model) %>% dplyr::mutate(tokens = dplyr::n())

    #out <- unique(as.data.frame(out,  stringsAsFactors = F))
    out <- out[order(out$score, decreasing = T),]
    rownames(out) <- 1:nrow(out)

  }

  if(normalize == "truemax"){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}

  if(normalize == "idealmax"){
    #compx <- cna:::getComplexity(sols)
    compx <- rep(mf$cx, mf$Freq)
    cfreqtab <- as.data.frame(table(compx))
    cfreqtab$compx <- as.integer(as.character(cfreqtab$compx))
    cfreqtab <- cfreqtab[order(cfreqtab$compx, decreasing = T),]

    cfreqtab$selfscore <- if(scoretype == "full"){
      (cfreqtab$Freq - 1) * 2
    } else {
      cfreqtab$Freq - 1
    }

    otherscore <- vector("integer", nrow(cfreqtab))

    if (scoretype == "supermodel"){

      for (i in seq_along(1:nrow(cfreqtab))) {
        tt <- cfreqtab[cfreqtab$compx > cfreqtab[i,]$compx,]
        otherscore[i] <- sum(tt$Freq)
      }
    }


    if (scoretype == "submodel"){

      for (i in seq_along(1:nrow(cfreqtab))) {
        tt <- cfreqtab[cfreqtab$compx < cfreqtab[i,]$compx,]
        otherscore[i] <- sum(tt$Freq)
      }
    }

    if (scoretype == "full"){
      for (i in seq_along(1:nrow(cfreqtab))) {
        otherscore[i] <- sum(cfreqtab[-i,]$Freq)
      }
    }

    cfreqtab$otherscore <- otherscore

    idealmaxscore <- max(cfreqtab$selfscore + cfreqtab$otherscore)

    if (max(out$score>=1)){out$score <- out$score / idealmaxscore}

  }

  return(structure(list(models = out,
                        verbose = if(verbose){scsums}else{NULL},
                        print.all = print.all,
                        scoretype = scoretype,
                        normal = normalize,
                        maxsols = list(maxsols = maxsols, excluded = excluded_sols)
  ), class = "frscore"))

}


subAdd <- function(x, y){
  re <- is.submodel(x,y)
  return(data.frame(names(re), attributes(re)$target, ifelse(re[[1]] == TRUE, 1, NA), checked = 1))
}

# Print method for frscore()
#' @export
#' @importFrom utils head
print.frscore <- function(x, verbose = x$verbose, print.all = x$print.all, maxsols = x$maxsols, ...){
  cat("FRscore, score type:", x$scoretype, "||", "score normalization:", x$normal, "\n\n")
  if(maxsols$maxsols == "ignored"){
    cat("no submodel checks were needed, argument 'maxsols' ignored \n")
  } else {
    cat("maxsols set to", maxsols$maxsols, "--", maxsols$excluded, "solution types excluded from scoring \n\n")
  }
  cat("-----\n \n")
  cat("Model types: \n")
  cat("\n")
  if(print.all){
    print(x$models)
  } else {
    print(head(x$models, n = 20L))
    cat("\n")
    nr <- nrow(x$models) - 20L
    if(nr > 0){cat('...there were', nr, 'more model types, use \'print.all = TRUE\' to print all \n')}
    cat('\n')

  }

  if(is.null(verbose)){invisible(x)} else {
    cat('\n')
    cat('Score composition: \n')
    cat('----- \n \n')
    print(x$verbose)
    invisible(x)
  }
}

