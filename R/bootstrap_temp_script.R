#
#
# boot_demo <- function(data, times, con, cov, outcome = TRUE){
#   boot_res <- vector("list", times)
#   for (i in seq_along(boot_res)){
#     r <- sample(1:nrow(data), nrow(data), replace = TRUE)
#     resample <- d.error[r,]
#     boot_res[[i]] <- tryCatch(csf(cna(resample,
#                                       con = con,
#                                       cov = cov,
#                                       outcome = outcome,
#                                       strict = TRUE))[,"condition"],
#                               error = function(e) NULL)
#   }
#   out <- unlist(boot_res)
#   #out <- data.frame(table(out))
#   #colnames(out) <- c("model", "tokens")
#   #out <- out[order(out$tokens, decreasing = T),]
#   return(out)
# }

# # resample `d.error` and analyze with con=cov=0.9
# resampling_ex1 <- suppressMessages(boot_demo(d.error,
#                                              100,
#                                              outcome = "E",
#                                              con = 1,
#                                              cov = 1))
# # rownames(resampling_ex1) <- 1:nrow(resampling_ex1)
# # resampling_ex1
#
# frscore(resampling_ex1, maxsols = 100)
