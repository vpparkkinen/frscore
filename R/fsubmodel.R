
library(cna)

# Minimal implementation for asf in standard form:
# Note that the rhs is NOT considered at all, such that 
#   fsubmodel_asf("A+B<->C", "A+B<->D") returns TRUE.
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


# Minimal implementation for csf in standard form:
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

# Application of fsubmodel_csf():
y <- "(A+B*c<->X)*(C+d<->Y)"  # reference model
x <- c("(A<->X)*(C<->Y)",        # submodel
       "(B<->X)*(d<->Y)",        # submodel
       "(A+c<->X)*(C+d<->Y)",    # submodel
       "A+B*c<->X",              # submodel
       "(A+B*c<->X)*(C+d<->Y)",  # submodel (identical)
       "(C<->X)*(C+d<->Y)",      # no submodel
       "A+B*c<->Z",              # no submodel
       "(A+B*c<->Z)*(C+d<->Y)")  # no submodel

fsubmodel_csf(x, y)
is.submodel(x, y)

all(fsubmodel_csf(x, y) == is.submodel(x, y))

library(microbenchmark)
microbenchmark(
  slow = is.submodel(x, y), 
  fast = fsubmodel_csf(x, y))
# -> fsubmodel_csf() is ~4 times faster in this example

