subCounter <- function(s,p){
  if (is.submodel(s[,1],p[,1])){
    return(data.frame(mod=s[,1], subsc=p[,2], supmod=p[,1], supsc=s[,2], stringsAsFactors=FALSE))
  } else
    return(data.frame(mod=s[,1], subsc=0, supmod=p[,1], supsc=0, stringsAsFactors=FALSE))
  }



frscore <- function(sols, normalize = F, verbose = F, scoretype = "full"){
  if (typeof(sols) != "character"){
    stop("sols should be a character vector of CCM solutions, not object of type ", typeof(sols))}
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
      
      #out <- sols
      #out <- as.character(out)
      #if (length(out) == 1) {out <- data.frame(condition = out[order(out$condition),])} else {
      sols <- sols[order(sols)]
      
      mf <- as.data.frame(table(sols), stringsAsFactors = FALSE)
        
      if (nrow(mf)==1){
        if(scoretype %in% c("submodel", "supermodel")){
          out <- rep((mf$Freq-1)*2/2, mf$Freq)
          
        }else{
          out <- rep((mf$Freq-1)*2, mf$Freq)
          
        }
        out <- data.frame(model = sols, score = out)
        #names(out) <- sols
        #if(weigh){out$score <- out$score*out$concov}
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
        
        #sc <- rbind.fill(lapply(sscore, function(y) rbind.fill(y)))
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
        
        # if (scoretype == "full") {out$score <- rep(pre.ssc$subsc, mf$Freq) + rep(pre.susc$supsc, mf$Freq) +
        #   (rep(mf$Freq, mf$Freq)-1)*2}
        # 
        # if (scoretype == "supermodel") {out$score <- rep(pre.ssc$subsc, mf$Freq)  +
        #   (rep(mf$Freq, mf$Freq)-1)*2/2}
        # 
        # if (scoretype == "submodel") {out$score <- rep(pre.susc$supsc, mf$Freq) +
        #   (rep(mf$Freq, mf$Freq)-1)*2/2}
        # 
        if (scoretype == "full") {out <- rep(pre.ssc$subsc, mf$Freq) + rep(pre.susc$supsc, mf$Freq) +
          (rep(mf$Freq, mf$Freq)-1)*2}
        
        if (scoretype == "supermodel") {out <- rep(pre.ssc$subsc, mf$Freq)  +
          (rep(mf$Freq, mf$Freq)-1)*2/2}
        
        if (scoretype == "submodel") {out <- rep(pre.susc$supsc, mf$Freq) +
          (rep(mf$Freq, mf$Freq)-1)*2/2}
        
        out <- data.frame(model = sols, score = out)
        #names(out) <- sols
        
        
        if(normalize){if (max(out$score>=1)){out$score <- out$score / max(out$score)}}
        if(verbose==TRUE){return(list(out, scsums))}else{
          return(out)}}
    }
}





# asf0 <- structure(list(condition = c("AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+lb*YY+rd*VD<->OUT", "AE*JM*RK+AE*LB*RK+AE*OG*RK+AE*RK*vh+JM*lb*YY+lb*OG*YY+lb*vh*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*JM*RK+AE*RK*YY+lb*vh*YY+mo*vh*YY<->OUT", "AE*RK+AE*ZF+lb*MO*YY+lb*OG*YY<->OUT", 
# "AE*JM*RK*VH+AE*LB*RK+AE*RK*YY+lb*vh*YY<->OUT", "AE*RK*VW+AE*RK*zf+lb*vh*YY+lb*YY*ZF<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*JM*RK+AE*LB*OE*RK+AE*RK*YY+lb*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*JM*RK+AE*RK*VW+JM*lb*YY+lb*OG*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*RK*ZD+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*RK*ZD+lb*YY*ZF<->OUT", 
# "AE*RK*te+AE*RK*VD+AE*RK*YY+AE*VD*ZF+AE*YY*ZF+lb*RK*te+lb*RK*VD+lb*RK*YY+lb*VD*ZF+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*MO*YY+lb*OG*YY<->OUT", "AE*RK+lb*YY+rd*VD<->OUT", 
# "AE*LB*RK+AE*LB*ZF+JM*lb*RK*zf+lb*RK*YY+lb*YY*ZF<->OUT", "AE*DJ*RK+AE*DJ*ZF+AE*RK*yy+AE*yy*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+lb*YY+og*rd*VD+rd*VD*zd<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*RK*ZD+lb*YY*ZF<->OUT", "AE*JM*RK+AE*LB*RK+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*JM*RK+AE*RK*VW+lb*OG*YY+lb*RD*YY<->OUT", "AE*RK+AE*ZF+JM*lb*YY+lb*vh*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*DV*RK+AE*RK*zf+lb*YY+rd*VD<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*OG*YY+lb*RD*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*JM*RK+AE*LB*RK+lb*OG*YY+lb*RD*YY<->OUT", "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*MO*YY+lb*OG*YY<->OUT", "AE*DJ*RK+AE*DJ*ZF+AE*RK*yy+AE*yy*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*JM*RK+AE*LB*RK+lb*OG*YY+lb*RD*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK*VW+AE*RK*zf+JM*lb*YY+lb*OG*YY<->OUT", "AE*DJ*YY*ZF+AE*kw*ZF+AE*RK*zf+DJ*VD*YY*ZF+kw*VD*ZF+RK*VD*zf<->OUT", 
# "AE*RK+lb*YY+rd*VD<->OUT", "AE*JM*RK+AE*LB*RK+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*lb+AE*RK+DJ*DV*rd*VD+lb*YY+RK*YY<->OUT", "AE*JM*RK+AE*RK*VW+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", "AE*RK+AE*ZF+lb*OG*YY+lb*RD*YY<->OUT", 
# "ae*lb*VW*YY+AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*JM*RK*VH+AE*LB*RK+AE*RK*YY+lb*vh*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", "AE*JM*RK+AE*RK*VW+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*LB*RK+AE*RK*vh+ES*lb*YY+JM*lb*YY+lb*vh*YY<->OUT", 
# "AE*DJ*RK+AE*RK*zf+lb*vh*YY+mo*vh*YY<->OUT", "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*RK*ZD+lb*YY*ZF<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*JM*RK+AE*RK*VW+JM*lb*YY+lb*OG*YY<->OUT", "AE*RK+AE*ZF+JM*lb*YY+lb*vh*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*DJ*RK+AE*RK*zf+lb*RD*YY+lb*YY*zd<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*JM*RK+AE*RK*YY+lb*RD*YY+lb*vh*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*ES*RK+AE*JM*RK+AE*KW*RK*VW+ES*lb*YY+JM*lb*YY+KW*lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+lb*YY+og*rd*VD+rd*VD*zd<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*MO*YY+lb*OG*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*OG*YY+lb*RD*YY<->OUT", "AE*RK+AE*ZF+JM*lb*YY+lb*OG*YY<->OUT", 
# "AE*DV*RK+AE*RK*zf+lb*YY+rd*VD<->OUT", "AE*JM*RK+AE*RK*VW+lb*OG*YY+lb*RD*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*ES*RK+AE*JM*RK+lb*MO*YY+lb*vh*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK*VW+AE*RK*zf+JM*lb*YY+lb*OG*YY<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*JM*RK+AE*RK*VW+JM*lb*YY+lb*OG*YY<->OUT", "AE*JM*RK+AE*OG*RK*YY+AE*RK*VW+JM*lb*YY+lb*OG*YY+lb*VW*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", "AE*RK*VW+AE*RK*zf+JM*lb*YY+lb*OG*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK*vh+lb*vh*ZF<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*JM*RK+JM*lb*RK+lb*YY<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+lb*RK*YY<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK+vh*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", "AE*RK+lb*RK*ZD<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*RK*ZD<->OUT", "AE*RK<->OUT", 
# "AE*RK+OG*VD*zd<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*JM*RK+AE*RK*YY+JM*lb*RK+lb*RK*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK+lb*VD<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK+vh*YY<->OUT", 
# "AE*RK+lb*RK*YY<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "JM*RK*zf+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*RK*ZD<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK*vh+lb*vh*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*JM*RK+JM*lb*RK+lb*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", "AE*RK+lb*RK*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+vh*YY<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", 
# "AE*RK+lb*RK*ZD<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*RK*ZD<->OUT", 
# "AE*RK<->OUT", "AE*RK+OG*VD*zd<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*JM*RK+AE*RK*YY+JM*lb*RK+lb*RK*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK+lb*VD<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK+vh*YY<->OUT", 
# "AE*RK+lb*RK*YY<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", 
# "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", "AE*RK<->OUT", "AE*RK+AE*ZF+lb*YY<->OUT", 
# "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", "AE*RK+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "JM*RK*zf+lb*YY<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK<->OUT", 
# "AE*RK<->OUT", "AE*RK<->OUT", "AE*RK+lb*RK*ZD<->OUT", "AE*RK+AE*ZF+lb*RK*YY+lb*YY*ZF<->OUT", 
# "AE*RK<->OUT", "AE*RK+AE*ZF+lb*RK+lb*ZF<->OUT", "AE*RK<->OUT", 
# "AE*RK+AE*ZF+lb*YY<->OUT")), class = "data.frame", row.names = c(NA, 
# -300L))
# 
# subScore(asf0,normalize = T) 
