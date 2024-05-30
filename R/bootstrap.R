frscored_cna_bootstrap.data.frame <- function(x,
                                             ...,
                                             bootstrap_samples = 100){
  args <- list(...)
  boot_res <- vector("list", bootstrap_samples)
  for (i in seq_along(boot_res)){
    r <- sample(1:nrow(data), nrow(data), replace = TRUE)
    resample <- d.error[r,]
    boot_res[[i]] <- tryCatch(csf(cna(resample,
                                      con = con,
                                      cov = cov,
                                      outcome = outcome,
                                      strict = TRUE))[,"condition"],
                              error = function(e) NULL)
  }
}
