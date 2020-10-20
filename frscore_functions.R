##helper function for frscore()

subCounter <- function(s,p){
  if (is.submodel(s[,1],p[,1])){
    return(data.frame(mod=s[,1], subsc=p[,2], supmod=p[,1], supsc=s[,2], stringsAsFactors=FALSE))
  } else
    return(data.frame(mod=s[,1], subsc=0, supmod=p[,1], supsc=0, stringsAsFactors=FALSE))
  }


## frscore() calculates the fr-scores for a set of models 

frscore <- function(sols, normalize = F, verbose = F, scoretype = "full"){
  if (typeof(sols) != "character"){
    stop("sols should be a character vector of CNA solutions, not object of type ", typeof(sols))}
  if (NA %in% sols){sols <- sols[!is.na(sols)]}
  if (length(sols) == 0){stop("nothing to test")} else 
    if(length(sols) == 1){
      out <- data.frame(model = sols, score = 0L)
      #names(out) <- sols
      if(verbose){
        scsums <- list(NULL)
        names(scsums) <- sols
        return(list(out, scsums))  
      }else{return(out)}
    }else{
      
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
          #dupscores <- lapply(dupscores, function(x) x %>% rename(sols = model))
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
        
        out <- data.frame(model = sols, score = out)
  
        
        if(normalize){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}
        if(verbose==TRUE){return(list(out, scsums))}else{
          return(out)}}
    }
}



## frscored_cna() performs a reanalysis series on a data set with all combinations of 
## fit threshold values that can be generated by varying consistency and coverage in a given range 
## by a constant value, determined respectively by arguments 'fit.range' and 'granularity',
## and calculates fr-scores for the models returned in the analyses.
## If a candidate model is provided as 'test.model', fr-score will be calculated also for that model relative
## to the models returned in the reanalysis series, regardless of whether the candidate model
## is returned in the reanalysis series or not.

frscored_cna <- function(x, 
                         fit.range = c(1, 0.7), 
                         granularity = 0.05, 
                         output = "csf", 
                         normalize = TRUE, 
                         verbose = FALSE,
                         test.model = NULL,
                         ...){
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not meaningful")
  }
  cl$fit.range <- cl$granularity <- cl$normalize <- cl$verbose <- cl$scoretype <- cl$test.model <- NULL
  cl[[1]] <- as.name("rean_cna")
  attempt <- seq(max(fit.range), min(fit.range), -granularity)
  cl$attempt <- attempt
  cl$output <- output
  clres <- eval(cl)
  rescomb <- do.call(rbind, clres)
  rescomb <- rescomb[!is.na(rescomb[,1]),] 
  rescomb$condition <- gsub("\\),\\(", "\\)*\\(", as.character(rescomb$condition))
  if (is.null(test.model)){
    scored <- frscore(rescomb$condition, normalize = normalize, verbose = verbose)
    if (verbose){scored[[1]] <- unique(scored[[1]][order(scored[[1]]$score, decreasing = T),])} else {
      scored <- unique(scored[order(scored$score, decreasing = T),])
    }
    #rescomb$frscore <- scored$score
    return(list(scored, rescomb))
  } else {
    if(any(sapply(rescomb$condition, function(x) identical.model(x, test.model)))){
      scored <- frscore(rescomb$condition, normalize = normalize, verbose = verbose)
    } else {
      scored <- frscore(c(rescomb$condition, test.model), normalize = normalize, verbose = verbose)
    }
    if (verbose) {scored[[1]] <- scored[[1]][order(scored[[1]]$score, decreasing = T),]
    tested <- scored[sapply(scored[[1]]$model, function(x) identical.model(x, test.model)),]} else{
      scored <- scored[order(scored$score, decreasing = T),]
      tested <- scored[sapply(scored$model, function(x) identical.model(x, test.model)),]
    }
    return(list(tested, unique(scored), rescomb))
  }
}


## rean_cna() performs a reanalysis series based on which fr-scores can be calculated
## output = "asf" returns asfs, output = "csf" csfs.

rean_cna <- function(..., what = "c", 
                   attempt = seq(1, 0.7, -0.05), ncsf = 20, output = c("csf", "asf")){
  
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not meaningful")
  }
  cl$attempt <- cl$asf <- cl$ncsf <- cl$csf <- cl$output <- NULL
  cl[[1]] <- as.name("cna")
  ccargs <- as.data.frame(expand.grid(attempt, attempt))
  colnames(ccargs)<-c("lowfirst", "lowsec")
  
  sols <- vector("list", length = nrow(ccargs))
  for (i in 1:length(sols)){
      cl$con <- ccargs[i,"lowfirst"]
      cl$cov <- ccargs[i, "lowsec"]
    if (output[[1]]=="csf"){sols[[i]] <- csf(eval(cl), n = ncsf)} 
    if (output[[1]]=="asf"){sols[[i]] <- asf(eval(cl))}
    dt <- data.frame(cnacon = rep(cl$con, nrow(sols[[i]])), 
                                  cnacov = rep(cl$cov, nrow(sols[[i]])))
    sols[[i]] <- cbind(sols[[i]], dt)
  }
  return(sols)
}




