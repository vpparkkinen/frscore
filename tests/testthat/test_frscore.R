
local_edition(2)

inputmodels <- readRDS(testthat::test_path("frscore_cortest_input.RDS"))




#                     function(x) frscore(x,
#                                 normalize = 'none',
#                                 maxsols=Inf))






test_that("Raw scores are correct for cs models",{
  rawscores <- lapply(inputmodels,
                      function(x) frscore(x,
                                          normalize = 'none',
                                          maxsols = Inf))
  expect_known_hash(rawscores, "32a7fcb72b")

})



mv_all <- readRDS(system.file("tests", "testthat","frscore_mvcortest_all.RDS",
                              package = "frscore"))

mv_res <- lapply(mv_all, '[[', 2)

scores_mv <- lapply(mv_all, function(x) {test <- x[[1]]; eval(x[[3]])})

test_that("Scores are correct for mv models",{
 expect_equal(scores_mv, mv_res)
})

test_that("normalization methods work",{
 expect_known_hash(frscore(inputmodels[[30]],
                           normalize = "idealmax",
                           maxsols = Inf), hash = "9f00210680")
 expect_known_hash(frscore(inputmodels[[30]],
                           normalize = "truemax",
                           maxsols = Inf), hash = "ac2f7b6deb")
 })

test_that("syntax standardization works",{
  expect_identical(stxstd("(A*d+C*a+c*E*g<->B)*(E+B*g*h<->A)"),
                   "(B*g*h+E<->A)*(A*d+C*a+E*c*g<->B)")
  expect_error(stxstd("(A*d+C*a+c*E*g<->B)*m(E+B*g*h<->A)"), regexp = "Invalid")
  expect_error(stxstd(c("(A*d+C*a+c*E*g<->B)*(E=1+B*g*h<->A)",
                        "(A*d+C*a+c*E*g<->B)*(E+B*g*h<->A)")),
               regexp = "appear")
  })


ss <- structure(c("A*c+A*D+B*C<->E", "A*c+A*D+B*C<->E", "A+B*C<->E",
                  "A+B<->E", "A*c+A*D+B*C<->E", "A*c+A*D+B*C<->E", "A+B*C<->E",
                  "A+B<->E", "A*c+B*C<->E", "A*B+A*D+B*C<->E", "A*c+B*C<->E", "A*B+A*D+B*C<->E",
                  "A+B*C<->E", "(E<->A)*(A+B<->E)", "(E<->A)*(A+C*D<->E)", "A*c+B*C<->E",
                  "A*D+B*C<->E", "A*B+A*c+A*D<->E", "A*c+B*C<->E", "A*D+B*C<->E",
                  "A*B+A*c+A*D<->E", "(c*E+D*E<->A)*(A<->E)", "A<->E", "E<->A",
                  "(E<->A)*(B+C*D<->E)"), class = "character")


test_that("verbose output is correct",{
  expect_known_hash(frscore(ss, verbose = T), hash = "e10c2bfabc")
})



