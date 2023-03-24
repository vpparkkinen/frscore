


#' @importFrom cna C_is_submodel
fsubmodel_asf <- function(x, y){
  stopifnot(length(y) == 1)
  # transform asfs to structured lists
  a1 <- hstrsplit(lhs(x), c("+", "*"), relist = FALSE)
  a2 <- hstrsplit(lhs(y), c("+", "*"), relist = FALSE)
  # convert char -> int
  nms <- unique(c(a1, a2))
  b1 <- match(a1, nms)
  b2 <- match(a2, nms)
  # sort within conjunction
  conjlen1 <- attr(a1, "lengths")[[2]]
  iconj <- rep.int(seq_along(conjlen1), conjlen1)
  b1[] <- b1[order(iconj, b1)]
  conjlen2 <- attr(a2, "lengths")[[2]]
  iconj <- rep.int(seq_along(conjlen2), conjlen2)
  b2[] <- b2[order(iconj, b2)]
  # relist
  b1 <- relist1(relist1(b1, conjlen1), attr(a1, "lengths")[[1]])
  b2 <- relist1(b2, conjlen2)
  C_is_submodel(b1, b2, strict = FALSE)
}


fsubmodel_csf <- function(x, y){
  stopifnot(length(y) == 1)
  yy <- extract_asf(y)[[1]]
  xx <- extract_asf(x)
  lhsy <- setNames(lhs(yy), rhs(yy))
  if (length(x) == 0) return(logical(0))
  ux <- unlist(xx)
  llx <- lengths(xx)
  lux <- lhs(ux)
  rux <- rhs(ux)
  ok <- rep(FALSE, sum(lengths(xx)))
  for (r in intersect(names(lhsy), rux)) {
    ok[rux == r] <- fsubmodel_asf(lux[rux == r], lhsy[[r]])
  }
  as.vector(rowsum(1-ok, rep.int(seq_along(llx), llx)) == 0)
}



