frscored_cna <- function(x, 
                         fit.range = c(1, 0.7), 
                         granularity = 0.05, 
                         output = "csf", 
                         normalize = TRUE, 
                         verbose = F,
                         test.model = NULL,
                         ...){
  cl <- match.call()
  dots <- list(...)
  if (any(c("cov", "con", "con.msc") %in% names(dots))){
    stop("cna arguments 'con', 'cov', 'con.msc' not applicable")
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
    rescomb$frscore <- scored$score
    return(rescomb)
  } else {
    scored <- frscore(c(rescomb$condition, test.model), normalize = normalize, verbose = verbose)
    tested <- scored[sapply(scored$model, function(x) identical.model(x, test.model)),]
    return(list(tested, unique(scored)))
  }
}


