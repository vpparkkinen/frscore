library(cna)
library(dplyr)
library(Rfast)
library(tidyr)

##helper function for frscore()



subAdd <- function(x, y){
  re <- is.submodel(x,y)
  return(data.frame(names(re), attributes(re)$target, ifelse(re[[1]] == TRUE, 1, NA), checked = 1))
}



## frscore() calculates the fr-scores for a set of models 

frscore <- function(sols, 
                    #normalize = TRUE, 
                    normalize = c("truemax", "idealmax", "none"), 
                    verbose = FALSE, 
                    scoretype = c("full", "supermodel", "submodel"),
                    maxsols = 50,
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
    
    compsplit <- mf %>% group_split(cx)
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
    compsplit <- mf %>% group_split(cx)
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
    
      prescore <- prescore %>% filter(mod != supmod)
      prescore <- prescore %>% left_join(mf[,1:2], by = c("mod" = "sols")) %>% 
        mutate(supsc = Freq) %>% select(-Freq)
      prescore <- prescore %>% left_join(mf[,1:2], by = c("supmod" = "sols")) %>% 
        mutate(subsc = Freq) %>% select(-Freq)
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
      bysup <- bs %>% dplyr::group_split(supmod)
      supnames <- unlist(lapply(bysup, function(x) unique(x$supmod)))
      names(bysup) <- supnames
      subspermod <- lapply(bysup, function(x) x[,c(1,3)])
      subspermod <- lapply(subspermod, function(x) as.data.frame(x, stringsAsFactors = FALSE))
      subspermod <- lapply(subspermod, function(x) x[order(x$model),])
      
      sps <- sc[, c(1,2,3)]
      sps <- data.frame(supermodel = sps[,3], sup.frequency = sps[,2], mod = sps[,1])
      bysub <- sps %>% dplyr::group_split(mod)
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
                            dplyr::mutate(sub.frequency=Freq-1, sup.frequency = Freq-1, Freq = NULL))
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
    
    pre.ssc <- sc[,c(1,2)] %>% dplyr::group_by(mod) %>% dplyr::mutate(subsc = sum(subsc)) %>% dplyr::distinct()  
    pre.susc <- sc[,c(3,4)] %>% dplyr::group_by(supmod) %>% dplyr::mutate(supsc = sum(supsc)) %>% dplyr::distinct()  
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




# Print method for frscore()

print.frscore <- function(x, verbose = x$verbose, print.all = x$print.all, maxsols = x$maxsols){
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

## frscored_cna() performs a reanalysis series on a data set with all combinations of 
## fit threshold values that can be generated by varying consistency and coverage in a given range 
## by a constant value, determined respectively by arguments 'fit.range' and 'granularity',
## and calculates fr-scores for the models returned in the analyses.
## If a candidate model is provided as 'test.model', result for that model will be printed separately
## (assuming one uses print.frscored_cna()), provided the model is found in the reanalysis series, if not,
## the function stops.

frscored_cna <- function(x, 
                         fit.range = c(1, 0.7), 
                         granularity = 0.1, 
                         output = c("csf", "asf"),
                         scoretype = c("full", "supermodel", "submodel"),
                         normalize = c("truemax", "idealmax", "none"), 
                         verbose = FALSE,
                         test.model = NULL,
                         print.all = FALSE,
                         maxsols = 50,
                         ...){
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not meaningful")
  }
  cl$fit.range <- cl$granularity <- cl$normalize <- 
    cl$verbose <- cl$scoretype <- 
    cl$test.model <- cl$print.all <-  
    cl$scoretype <- cl$maxsols <- NULL
  cl[[1]] <- as.name("rean_cna")
  attempt <- seq(max(fit.range), min(fit.range), -granularity)
  cl$attempt <- attempt
  cl$output <- match.arg(output)
  clres <- eval.parent(cl)
  rescomb <- do.call(rbind, clres)
  rescomb <- rescomb[!is.na(rescomb[,1]),] 
  rescombtemp <- rescomb
  rescomb <- rescomb[,-c(which(names(rescomb) %in% c("cnacon", "cnacov")))]
  rescomb$condition <- as.character(rescomb$condition)
  rescomb$condition <- gsub("\\),\\(", "\\)*\\(", as.character(rescomb$condition))
  scoretype <- match.arg(scoretype)
  normalize <- match.arg(normalize)
  if (is.null(test.model)){
    scored <- frscore(rescomb$condition, normalize = normalize,
                      verbose = verbose, scoretype = scoretype,
                      maxsols = maxsols)
    if(is.null(scored)){warning('no solutions found in reanalysis series, perhaps consider lower fit range \n \n')
      return(NULL)}
  } else {
    if(any(sapply(rescomb$condition, function(x) identical.model(x, test.model)))){
      scored <- frscore(rescomb$condition, normalize = normalize, 
                        verbose = verbose, scoretype = scoretype,
                        maxsols = maxsols)
      if(is.null(scored)){warning('no solutions found in reanalysis series, perhaps consider lower fit range \n \n')
      return(NULL)}
    } else {
      stop('test.model not found in reanalysis series')
      }
    }
    
    sc <- scored[[1]]
    names(sc)[names(sc) == "model"] <- "condition"
    rescombXscored <- dplyr::left_join(rescomb, sc, by="condition") %>% 
      filter(!is.na(score))
    
    rescombXscored <- unique(rescombXscored)
    rescombXscored <- rescombXscored[order(rescombXscored$complexity, decreasing = T),]
    rescombXscored <- rescombXscored[order(rescombXscored$score, decreasing = T),]
    rownames(rescombXscored) <- 1:nrow(rescombXscored)
    
    if(!is.null(test.model)){
      tested <- rescombXscored[sapply(rescombXscored$condition, function(x) identical.model(x, test.model)),]
    } else {
      tested <- test.model
    }

  out <- structure(list(rean_models = rescombXscored, 
                        tested = tested, 
                        verbose = scored$verbose, 
                        print.all = print.all,
                        fit.range = fit.range,
                        granularity = granularity,
                        scoretype = scoretype,
                        normal = normalize,
                        rean.results = rescombtemp,
                        maxsols = scored$maxsols), 
                     class = c("frscored_cna", "list"))
  return(out)
}

# Print method for frscored_cna()

print.frscored_cna <- function(x, verbose = x$verbose, print.all = x$print.all, maxsols = x$maxsols){
  cat('FR-scored reanalysis series with fit range', x$fit.range[1], 'to', x$fit.range[2], 'with granularity', x$granularity, '\n')
  cat('Score type:', x$scoretype, '||', 'score normalization:', x$normal, '\n')
  if(maxsols$maxsols == "ignored"){
    cat("no submodel checks were needed, argument 'maxsols' ignored \n")
  } else {
    cat("maxsols set to", maxsols$maxsols, "--", maxsols$excluded, "solution types excluded from scoring \n\n")
  }
  cat('----- \n \n')
  if(!is.null(x$tested)){
    cat('Candidate model tested:', x$tested$condition, '\n \n')
    print(x$tested)
    cat('\n \n')
  }
  cat('Model types: \n \n')
  nr <- nrow(x$rean_models) - 20L
  if (print.all){
    print(x$rean_models)
  } else {
    print(head(x$rean_models, n = 20L))
    cat('\n')
    if(nr > 0){
      cat('...there were', nr, 'more scored model types, use \'print.all = TRUE\' to print all \n')
    }  
  }
  if(!is.null(verbose)){
    cat('\n')
    cat('Score composition: \n')
    cat('----- \n')
    
    print(verbose)
  }
  invisible(x)
}




## rean_cna() performs a reanalysis series based on which fr-scores can be calculated
## output = "asf" returns asfs, output = "csf" csfs. Primarily a helper function for frscored_cna().

rean_cna <- function(..., what = "c", 
                   attempt = seq(1, 0.7, -0.1), ncsf = 20, output = c("csf", "asf")){
  
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not meaningful")
  }
  output <- match.arg(output)
  cl$attempt <- cl$asf <- cl$ncsf <- cl$csf <- cl$output <- NULL
  cl[[1]] <- as.name("cna")
  ccargs <- as.data.frame(expand.grid(attempt, attempt))
  colnames(ccargs)<-c("lowfirst", "lowsec")
  
  sols <- vector("list", length = nrow(ccargs))
  for (i in 1:length(sols)){
      cl$con <- ccargs[i,"lowfirst"]
      cl$cov <- ccargs[i, "lowsec"]
    if (output=="csf"){sols[[i]] <- csf(eval.parent(cl), n = ncsf)} 
    if (output=="asf"){sols[[i]] <- asf(eval.parent(cl))}
    dt <- data.frame(cnacon = rep(cl$con, nrow(sols[[i]])), 
                                  cnacov = rep(cl$cov, nrow(sols[[i]])))
    sols[[i]] <- cbind(sols[[i]], dt)
  }
  return(structure(sols, class = c("rean_cna", "list")))
}




