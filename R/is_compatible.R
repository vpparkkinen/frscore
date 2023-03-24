library(cna)


case_flipper <- function(x){
  out <- ifelse(x == toupper(x), tolower(x), toupper(x))
  return(out)
}


decompose_model <- function(x){
  x <- noblanks(x)
  asfs <- unlist(extract_asf(x))
  rhss <- rhs(asfs)
  lhss <- lhs(asfs)
  out <- list(asfs = asfs, 
              rhss = rhss, 
              lhss = lhss)
  return(out)
}


ccheck_prep <- function(x,y){
  tar_decomp <- decompose_model(y)
  cand_decomp <- decompose_model(x)
  tar_asfs <- tar_decomp$asfs
  tar_outs <- tar_decomp$rhss
  tar_lhss <- tar_decomp$lhss
  cand_asfs <- cand_decomp$asfs
  cand_outs <- cand_decomp$rhss
  cand_lhss <- cand_decomp$lhss
  names(cand_lhss) <- cand_outs
  cand_facs <- unlist(lit_extract(cand_lhss), use.names = FALSE)
  not_subbable <- toupper(tar_outs) %in% c(toupper(cand_facs), 
                                           toupper(cand_outs))
  test_tar_lhss <- tar_lhss
  names(test_tar_lhss) <- tar_outs
  out <- list(target = y,
              target_lhss = test_tar_lhss,
              target_asfs = tar_asfs,
              candidate = x,
              candidate_lhss = cand_lhss,
              candidate_asfs = cand_asfs,
              no_sub = not_subbable)
  return(out)
}


is_compatible <- function(x, y, dat = NULL){
  x <- noblanks(x)
  y <- noblanks(y)
  out <- vector("logical", 1)
  attributes(out) <- list(why = "", 
                          x = x, 
                          y = y,
                          ultimate_asfs = NULL,
                          cand_asfs_checked = NULL,
                          og_y = y,
                          og_x = x)
  if(grepl("=", y)){
    if(!grepl("=", x)){stop("x and y must be same type of model")}
    type <- "mv"
  } else {
    type <- "bin"
  }
  if(is.null(dat) & type == "mv"){
    stop("A data frame, configTable, or a list specifying admissible \n
         factor values must be provided for multi-valued models")
  }
  if(type == "bin") {dat <- full.ct(y)}
  if(type == "mv") {dat <- full.ct(dat)}
  
  if(!is.inus(x, selectCases(y, x = dat))){
    out[1] <- FALSE
    attr(out, "why") <- "x is not inus wrt selectCases(y)"
    return(out)
  }
  is_sm <- fsubmodel_csf(x,y)
  c_asfcount <- unlist(strsplit(x, "<->"))
  is_x_csf <- length(c_asfcount) > 2
  if(!is_x_csf & is_sm){
    out[1] <- TRUE
    attr(out, "why") <- "x is asf and a submodel of y"
    return(out)
  }
  if(cyclic(x) || cyclic(y)){
    out[1] <- is_sm
    attr(out, "why") <- "x or y is cyclic, fall back on testing submodel relation"
    return(out)
  }
    
  
  if(is_x_csf & is_sm){
     x <- is_comp_subtar(x, dat = dat, type = type)
     attr(out, "why") <- "x is a csf and a submodel of y, substitute in x before checking compatibility"
  }
  out <- subin_target_ccomp(x , y , out, dat, type)
  return(out)
}

subin_target_ccomp <- function(x, y, out, dat = NULL, type){
  prepared <- ccheck_prep(x,y)
  prep_target <- prepared$target_lhss
  asf_subms <- fsubmodel_csf(prepared$candidate_asfs, y)
  cand_need_checking <- prepared$candidate_lhss[!asf_subms]
  subbed_tar_asfs <- vector("character", length(prepared$target_lhss))
  correct <- asf_subms
  names(correct) <- prepared$candidate_asfs

  for(i in seq_along(cand_need_checking)){
    subbed_tar_asfs[i] <- check_comp_asf(cand_need_checking[i], 
                                         prepared$target_lhss,
                                         prepared$no_sub,
                                         y,
                                         dat = dat,
                                         type = type)
    idx <- which(names(prepared$candidate_lhss) == names(cand_need_checking[i]))
    asf_cor <- fsubmodel_asf(prepared$candidate_asfs[idx],
                           subbed_tar_asfs[i])
    correct[names(correct) == prepared$candidate_asfs[idx]] <- asf_cor
  }
  attr(correct, "target") <- NULL

  out[1] <- all(correct)
  if(out[1]){
    attr(out, "why") <- "all x asfs are submodels of expanded y asfs"
  } else {
    attr(out, "why") <- "some x asfs are not submodels of expanded y asfs"
  }
  attr(out, "expanded_tar_asfs") <- subbed_tar_asfs
  attr(out, "cand_asfs_checked") <- correct
  return(out) 
}

extr_mv_facs <- function(x){
  fnames <- unlist(strsplit(x, 
                            "\\(|\\)|\\+|\\*|\\="))
  fnames <- fnames[sapply(fnames, function(x) grepl("\\D", x))]
  out <- unique(fnames)
  return(out)
}


check_comp_asf <- function(x, y, not_subbable, ogy, dat = NULL, type){
  if(type == "bin" & is.null(dat)){
    dat <- full.ct(ogy)
  }
  tar_lhss <- y
  tar_outs <- names(y)
  tar_outs_flipped <- sapply(tar_outs, case_flipper) #fix for mv
  cand_out <- names(x)
  outc_matches <- tar_outs %in% cand_out
  if(!any(outc_matches)){
    return(ogy)
  }
  outcome_match <- which(outc_matches)
  
  ultimate_lhs <- ultimate_lhs1 <- y[outcome_match]
  idx_sub_from <- vector("logical", length(tar_outs))

  while(any(sapply(toupper(tar_outs[!not_subbable]), 
            function(x) grepl(x, toupper(ultimate_lhs))))){
    sub_from_capmatch <- sapply(tar_outs, 
                                function(y) 
                                  grepl(y, ultimate_lhs))
    sub_from_capmatch[which(not_subbable)] <- FALSE
    sub_from_capflip <- sapply(tar_outs_flipped, 
                               function(y) 
                                 grepl(y, ultimate_lhs))
    sub_from_capflip[which(not_subbable)] <- FALSE
    subbing_from <- unique(c(which(sub_from_capmatch), which(sub_from_capflip))) 
    idx_sub_from[subbing_from] <- TRUE
    if(any(sub_from_capmatch)){
      for(i in which(sub_from_capmatch)){
        ultimate_lhs <- gsub(tar_outs[i],
                             paste0("(", tar_lhss[i], ")"),
                             ultimate_lhs)
      }  
    }
    if(any(sub_from_capflip)){
      for(i in which(sub_from_capflip)){
        ultimate_lhs <- gsub(tar_outs_flipped[i], 
                            paste0("!(", tar_lhss[i], ")"),
                            ultimate_lhs)
      }  
    }
  }

  if(identical(ultimate_lhs, ultimate_lhs1)){
    subbed_lhs <- ultimate_lhs1
  } else {
    if(type == "mv"){
      cond <- get_mvcond(lhs = ultimate_lhs, dat = dat)
    } else {
      cond <- getCond(selectCases(ultimate_lhs))
    }
    subbed_lhs <- rreduce(cond = cond, 
                          x = selectCases(ogy, x = dat), 
                          full = FALSE) 
  }
  out <- paste0(subbed_lhs, "<->", tar_outs[outcome_match])
  return(out)
}

get_mvcond <- function(lhs, dat){
  fnames <- extr_mv_facs(lhs)
  u_lhsdat <- selectCases(cond = lhs,
                          x = dat[,fnames])
  cond <- getCond(u_lhsdat)
  return(cond)
}


substitute_all <- function(x, dat = NULL, type){
  x <- decompose_model(x)
  subbed <- chain_substituter(x)
  lhss <- subbed$lhss
  if(type == "mv"){
    subbed$lhss <- unlist(lapply(lhss, function(x) get_mvcond(x, dat)))
  } else {
    subbed$lhss <- unlist(lapply(lhss, function(z) getCond(selectCases(z))))  
  }
  return(subbed)
}


chain_substituter <- function(x, 
                               subbed_from = vector("logical", length(x[[1]]))){
  sub_from_capmatch <- lapply(x$rhss, 
                              function(y) 
                                grepl(y, x$lhss))
  id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
  
  sub_from_capflip <- lapply(case_flipper(x$rhss), 
                             function(y) 
                               grepl(y, x$lhss))
  id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  while(any(c(id_sub_capflip, id_sub_capmatch))){
    for(i in seq_along(sub_from_capmatch)){
      if(id_sub_capmatch[i]){
        x$lhss[sub_from_capmatch[[i]]] <- gsub(x$rhss[i], 
                                               paste0("(", x$lhss[i], ")"), 
                                               x$lhss[sub_from_capmatch[[i]]])
      } 
    }
    for(i in seq_along(sub_from_capflip)){
      if(id_sub_capflip[i]){
        x$lhss[sub_from_capflip[[i]]] <- gsub(tolower(x$rhss[i]), 
                                              paste0("!(", x$lhss[i], ")"), 
                                              x$lhss[sub_from_capflip[[i]]])
      } 
    }
    
    wh_subbed_from <- which(subbed_from)
    wh_capmatch <- which(id_sub_capmatch)
    wh_capflip <- which(id_sub_capflip)
    w_subbed <- unique(c(wh_subbed_from, wh_capmatch, wh_capflip))
    subbed_from[w_subbed] <- TRUE
    sub_from_capmatch <- lapply(x$rhss, 
                                function(y) 
                                  grepl(y, x$lhss))
    id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
    
    sub_from_capflip <- lapply(case_flipper(x$rhss), 
                               function(y) 
                                 grepl(y, x$lhss))
    id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
  }
  x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
  return(x)
}


# chain_substituter <- function(x, 
#                               subbed_from = vector("logical", length(x[[1]]))){
#   sub_from_capmatch <- lapply(x$rhss, 
#                               function(y) 
#                                 grepl(y, x$lhss))
#   id_sub_capmatch <- unlist(lapply(sub_from_capmatch, any))
#   
#   sub_from_capflip <- lapply(case_flipper(x$rhss), 
#                              function(y) 
#                                grepl(y, x$lhss))
#   id_sub_capflip <- unlist(lapply(sub_from_capflip, any))
#   if(!any(c(id_sub_capflip, id_sub_capmatch))){
#     x <- lapply(x, function(y){y <- y[!subbed_from]; return(y)})
#     return(x)
#   }
#   for(i in seq_along(sub_from_capmatch)){
#     if(id_sub_capmatch[i]){
#       x$lhss[sub_from_capmatch[[i]]] <- gsub(x$rhss[i], 
#                                               paste0("(", x$lhss[i], ")"), 
#                                               x$lhss[sub_from_capmatch[[i]]])
#     } 
#   }
#   for(i in seq_along(sub_from_capflip)){
#     if(id_sub_capflip[i]){
#       x$lhss[sub_from_capflip[[i]]] <- gsub(tolower(x$rhss[i]), 
#                                              paste0("!(", x$lhss[i], ")"), 
#                                              x$lhss[sub_from_capflip[[i]]])
#     } 
#   }
#   wh_subbed_from <- which(subbed_from)
#   wh_capmatch <- which(id_sub_capmatch)
#   wh_capflip <- which(id_sub_capflip)
#   w_subbed <- unique(c(wh_subbed_from, wh_capmatch, wh_capflip))
#   subbed_from[w_subbed] <- TRUE 
#   chain_substituter(x, subbed_from = subbed_from)
# }

is_comp_subtar <- function(x, y, type, dat){  
  x_expanded <- substitute_all(x, dat, type)
  x_expanded$lhss <- unlist(lapply(x_expanded$lhss, 
                                   function(z) rreduce(cond = z, x = dat)))
  d <- data.frame(x_expanded$lhss, x_expanded$rhss)
  new_asfs <- do.call("paste", c(d, sep = "<->"))
  if(length(new_asfs) > 1){
    new_asfs <- paste0("(", new_asfs, ")")
  } 
  new_csf <- paste0(new_asfs, collapse = "*")
  return(new_csf)
} 


mvdatgen <- function(x){
  fct <- full.ct(x)
  fct_u <- apply(fct, 2, unique)
  mv_values <- lapply(fct_u, 
                      function(x) {if(length(unique(x)) < 3){
                        x <- min(x):(max(x)+(3-length(x)))
                      } else {
                        x <- x
                      }
                        return(x)})
  out <- full.ct(x = mv_values)
  return(out)
}

lit_extract <- function(lhs){
  d <- strsplit(lhs, "\\+")
  out <- lapply(d, function(x) strsplit(x, "\\*"))
  return(out)
}

print.is_compatible <- function(x){
  print(x[[1]])
}















