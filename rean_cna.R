rean_cna <- function(..., what = "c", 
                   attempt = seq(0.95, 0.75, -0.05), ncsf = 20, output = "csf"){
  
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not applicable")
  }
  cl$attempt <- cl$asf <- cl$ncsf <- cl$csf <- cl$output <- NULL
  cl[[1]] <- as.name("cna")
  ccargs <- as.data.frame(expand.grid(attempt, attempt))
  colnames(ccargs)<-c("lowfirst", "lowsec")
  
  sols <- vector("list", length = nrow(ccargs))
  for (i in 1:length(sols)){
      cl$con <- ccargs[i,"lowfirst"]
      cl$cov <- ccargs[i, "lowsec"]
    if (output=="csf"){sols[[i]] <- csf(eval(cl), n = ncsf)} 
    if (output=="asf"){sols[[i]] <- asf(eval(cl))}
    dt <- data.frame(cnacon = rep(cl$con, nrow(sols[[i]])), 
                                  cnacov = rep(cl$cov, nrow(sols[[i]])))
    sols[[i]] <- cbind(sols[[i]], dt)
  }
  return(sols)
}
