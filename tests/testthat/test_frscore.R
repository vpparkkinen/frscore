
local_edition(3)

mvdatgen <- function(x){
  fct <- full.ct(x)
  fct_u <- apply(fct, 2, unique)
  mv_values <- lapply(fct_u,
                      function(x) {if(length(unique(x)) < 2){
                        x <- min(x):(max(x)+(2-length(x)))
                      } else {
                        x <- x
                      }
                        return(x)})
  out <- full.ct(x = mv_values)
  return(out)
}

mv_all <- readRDS(testthat::test_path("frscore_mvcortest_all.RDS"))
mvdat <- mvdatgen("(D=2*E=1+C=3*D=3*E=1<->A=2)*(A=3*E=2+D=1*E=1<->B=2)")

test_that("frscore works for mv",{
 expect_snapshot(frscore(mv_all[[1]][[1]], dat = mvdat))
})


ss <- structure(c("A*c+A*D+B*C<->E", "A*c+A*D+B*C<->E", "A+B*C<->E",
                  "A+B<->E", "A*c+A*D+B*C<->E", "A*c+A*D+B*C<->E", "A+B*C<->E",
                  "A+B<->E", "A*c+B*C<->E", "A*B+A*D+B*C<->E", "A*c+B*C<->E", "A*B+A*D+B*C<->E",
                  "A+B*C<->E", "(E<->A)*(A+B<->E)", "(E<->A)*(A+C*D<->E)", "A*c+B*C<->E",
                  "A*D+B*C<->E", "A*B+A*c+A*D<->E", "A*c+B*C<->E", "A*D+B*C<->E",
                  "A*B+A*c+A*D<->E", "(c*E+D*E<->A)*(A<->E)", "A<->E", "E<->A",
                  "(E<->A)*(B+C*D<->E)"), class = "character")


test_that("frscore works for cs",{
  expect_snapshot(frscore(ss))
})


syntx <- c("z1p * cUe + EUF <-> 7Lv", "fqE * BqT + yq2 <-> M\\M", "1c * bJi + Hin <-> 2x",
           "l]C * NKT + H4M <-> TA0", "QXj * qfC + aHn <-> EdE", "mOb * gCy + 8f0 <-> w3l",
           "N0_ * YCj + 7`] <-> VK`", "ot7 * cGS + r9G <-> DLH", "HrW * SIY + F8L <-> 6rz",
           "ZVB * AMk + K0^ <-> 1I5", "z1p * cUe + EUF <-> 7", "z1p * E <-> b",
           "N0 * YCj + 7 <-> VK", "X <-> x", "c * (bJi + H <-> x", "rZ * Yj ++ tL <-> goy",
           "rZ * Yj + tL <-> *goy")

syntx_exp <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
               FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)



syntxmv <- c("C=2*E=2+D=3*E=1+B=1*D=1*E=3+B=1*D=2*E=3<->A=2", "C=2*E=2+D=3*E=1+B=1*D=1*E=3+B1*D=2*E=3<->A=2",
  "B=1*E=3+C=2*E=2+D=3*E=1<->A=2", "B=1*E=3+C=2*E=2+B=2*C=2*D=3<->A=2",
  "A=2*C=3+C=3*E=1+A=3*D=3*E=1<->B=1", "B=1*E=3+C=2*E=2+B=2*C=2*D=3<->A=2",
  "A=2*E=3+C=3*E=1+A=3*D=3*E=1<->B=1", "A=2*C=3+C=3*E=1+_A=3*D=3*E=1<->B=1",
  "A=2*C=3+C=3*+E=1+A=3*D=3*E=1<->B=1", "A=2*C=3+C=3*E==1+A=3*D=3*E=1<->B=1",
  "A=2*C=3+C=3*E=1+A=3*D=3*E=1'<->B=1", "A=2*C=3+C=3*E=1+A=3*D=3*E=1->B=1",
  "A=2*c=3+C=3*E=1+A=3*D=3*E=1->B=1", "A=2*+C=3*E=1+A=3*D=3*E=1->B=1",
  "A=+C=3*E=1+A=3*D=3*E=1->B=1", "A=+C=3*E=1+A=3*D=3*!E=1->B=1",
  "A=1+C=3*E=1+A=3*D=3*!E=1->B=1", "(A=1)<->B=1", "1<->B=1", "A=22*C=33+C=37*+E=100+A=23*D=32*E=14<->B=100",
  "A=22*C=33+C=37*E=100+A=23*D=32*E=14<->B=100")

syntxmv_exp <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE,
                 FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, TRUE)

stxstd_test <- function(x){
  t <- tryCatch(stxstd(x), error = function(z) FALSE)
  tt <-if(isFALSE(t)) FALSE else TRUE
  tt
}

test_that("invalid model syntax is detected", {
  expect_equal(sapply(syntx, stxstd_test, USE.NAMES = F), syntx_exp)
  expect_equal(sapply(syntxmv, stxstd_test, USE.NAMES = F), syntxmv_exp)

  expect_error(stxstd(c("(A*d+C*a+c*E*g<->B)*(E=1+B*g*h<->A)",
                        "(A*d+C*a+c*E*g<->B)*(E+B*g*h<->A)")),
               regexp = "appear")

})


