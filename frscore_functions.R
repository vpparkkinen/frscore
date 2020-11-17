library(cna)
library(dplyr)

##helper function for frscore()

subCounter <- function(s,p){
  if (is.submodel(s[,1],p[,1])){
    return(data.frame(mod=s[,1], subsc=p[,2], supmod=p[,1], supsc=s[,2], stringsAsFactors=FALSE))
  } else
    return(data.frame(mod=s[,1], subsc=0, supmod=p[,1], supsc=0, stringsAsFactors=FALSE))
  }


## frscore() calculates the fr-scores for a set of models 

frscore <- function(sols, 
                    normalize = TRUE, 
                    verbose = FALSE, 
                    scoretype = c("full", "supermodel", "submodel"), 
                    print.all = FALSE){
  if (typeof(sols) != "character"){
    stop("sols should be a character vector of CNA solutions, not object of type ", typeof(sols))}
  if (NA %in% sols){sols <- sols[!is.na(sols)]}
  if (length(sols) == 0){warning('no solutions to test')
    return(NULL)} else 
    if(length(sols) == 1){
      out <- data.frame(model = sols, score = 0L, stringsAsFactors = FALSE)
      if(verbose){
        scsums <- list(NULL)
        names(scsums) <- sols
        return(return(structure(list(models = out, 
                                     verbose = scsums, 
                                     print.all = print.all, 
                                     scoretype = scoretype), class = "frscore")))
      }else{
        return(structure(list(models = out, verbose = NULL, print.all = print.all, scoretype = scoretype), class = "frscore"))
        }
    }else{
      
      scoretype <- match.arg(scoretype)
      sols <- sols[order(sols)]
      
      mf <- as.data.frame(table(sols), stringsAsFactors = FALSE)
        
      if (nrow(mf)==1){
        if(scoretype %in% c("submodel", "supermodel")){
          out <- rep((mf$Freq-1)*2/2, mf$Freq)
          
        }else{
          out <- rep((mf$Freq-1)*2, mf$Freq)
          
        }
        out <- data.frame(model = sols, score = out)

        if(normalize){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}
        if(verbose){
          elems <- (mf$Freq-1)*2
          if (scoretype %in% c("supermodel", "submodel")){elems <- elems / 2}
          names(elems) <- sols[1]
          
          scsums <- list(elems)
          names(scsums) <- sols[1]
          return(list(out, scsums))
        }else{
          return(out)
        }
      }else{
        
        mf <- mf[order(mf[,1]),]
        sscore <- vector("list", nrow(mf))
        
        for (m in 1:nrow(mf)){
          subres <- vector("list", nrow(mf[-m,]))
          for(mo in 1:nrow(mf[-m,])){
            subres[[mo]] <- if (nchar(mf[,1][m]) > nchar(mf[-m,][,1][mo])){
              data.frame(mod=mf[,1][m], subsc=0, supmod=mf[-m,][,1][mo], supsc=0, stringsAsFactors=FALSE)
            }else{
              subCounter(mf[m,], mf[-m,][mo,])
            }  
          } 
          sscore[[m]] <- subres
        }
        
        sc <- do.call(rbind, lapply(sscore, function(y) do.call(rbind, y)))
        
        if(verbose){
          bs <- sc[, c(1,3,4)]
          colnames(bs)[colnames(bs) == "supsc"] <- "sub.frequency"
          colnames(bs)[colnames(bs) == "mod"] <- "model"
          bysup <- bs %>% group_split(supmod)
          supnames <- unlist(lapply(bysup, function(x) unique(x$supmod)))
          names(bysup) <- supnames
          subspermod <- lapply(bysup, function(x) x[,c(1,3)])
          subspermod <- lapply(subspermod, function(x) as.data.frame(x, stringsAsFactors = FALSE))
          
          sps <- sc[, c(1,2,3)]
          sps <- data.frame(supermodel = sps[,3], sup.frequency = sps[,2], mod = sps[,1])
          bysub <- sps %>% group_split(mod)
          subnames <- supnames <- unlist(lapply(bysub, function(x) unique(x$mod)))
          names(bysub) <- subnames
          superpermod <- lapply(bysub, function(x) x[,2])
          superpermod <- lapply(superpermod, function(x) as.data.frame(x, stringsAsFactors = FALSE))
          
          robbasis <- mapply(cbind, subspermod, superpermod, SIMPLIFY = F)
          mfs <- mf
          colnames(mfs)[colnames(mfs) == "sols"] <- "model"
          dups <- lapply(names(robbasis), function(x) mfs[mfs[,1]==x,])
          dupscores <- lapply(dups, function(x) x %>% 
                                mutate(sub.frequency=Freq-1, sup.frequency = Freq-1, Freq = NULL))
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
        
        
        
        pre.ssc <- sc[,c(1,2)] %>% group_by(mod) %>% mutate(subsc = sum(subsc)) %>% distinct  
        pre.susc <- sc[,c(3,4)] %>% group_by(supmod) %>% mutate(supsc = sum(supsc)) %>% distinct  
        pre.ssc <- pre.ssc[order(pre.ssc$mod),]
        pre.susc <- pre.susc[order(pre.susc$supmod),]
        
        if (scoretype == "full") {out <- rep(pre.ssc$subsc, mf$Freq) + rep(pre.susc$supsc, mf$Freq) +
          (rep(mf$Freq, mf$Freq)-1)*2}
        
        if (scoretype == "supermodel") {out <- rep(pre.ssc$subsc, mf$Freq)  +
          (rep(mf$Freq, mf$Freq)-1)*2/2}
        
        if (scoretype == "submodel") {out <- rep(pre.susc$supsc, mf$Freq) +
          (rep(mf$Freq, mf$Freq)-1)*2/2}
        
        out <- data.frame(model = sols, score = out, stringsAsFactors = FALSE)
        out <- out %>% group_by(model) %>% mutate(tokens = n())
        out <- unique(as.data.frame(out))
        out <- out[order(out$score, decreasing = T),]
        rownames(out) <- 1:nrow(out)
        
        if(normalize){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}
        if(verbose==TRUE){return(structure(list(models = out, verbose = scsums, print.all = print.all, scoretype = scoretype), class = "frscore"))}else{
          return(structure(list(models = out, verbose = NULL, print.all = print.all, scoretype = scoretype), class = "frscore"))}}
    }
}

# Print method for frscore()

print.frscore <- function(x, verbose = x$verbose, print.all = x$print.all){
  cat("FRscore, score type:", x$scoretype,  "\n")
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
                         normalize = TRUE, 
                         verbose = FALSE,
                         test.model = NULL,
                         print.all = FALSE,
                         ...){
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not meaningful")
  }
  cl$fit.range <- cl$granularity <- cl$normalize <- cl$verbose <- cl$scoretype <- cl$test.model <- cl$print.all <-  cl$scoretype <- NULL
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
  if (is.null(test.model)){
    scored <- frscore(rescomb$condition, normalize = normalize, verbose = verbose, scoretype = scoretype)
    if(is.null(scored)){cat('no solutions found in reanalysis series, perhaps consider lower fit range \n \n')
      return(NULL)}
  } else {
    if(any(sapply(rescomb$condition, function(x) identical.model(x, test.model)))){
      scored <- frscore(rescomb$condition, normalize = normalize, verbose = verbose, scoretype = scoretype)
      if(is.null(scored)){cat('no solutions found in reanalysis series, perhaps consider lower fit range \n \n')
      return(NULL)}
    } else {
      stop('test.model not found in reanalysis series')
      }
    }
    
    sc <- scored[[1]]
    names(sc)[names(sc) == "model"] <- "condition"
    rescombXscored <- left_join(rescomb, sc, by="condition")
    rescombXscored <- unique(rescombXscored)
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
                        rean.results = rescombtemp), 
                     class = c("frscored_cna", "list"))
  return(out)
}

# Print method for frscored_cna()

print.frscored_cna <- function(x, verbose = x$verbose, print.all = x$print.all){
  cat('FR-scored reanalysis series with fit range', x$fit.range[1], 'to', x$fit.range[2], 'with granularity', x$granularity, '\n')
  cat('Score type:', x$scoretype, '\n')
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
      cat('...there were', nr, 'more model types found, use \'print.all = TRUE\' to print all \n')
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
  return(sols)
}




