
local_edition(2)

d <- structure(list(A = c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L),
               B = c(1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L),
               C = c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L),
               D = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L),
               E = c(1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)),
          row.names = c("10", "13", "15", "18", "19", "22", "23", "27"),
          class = "data.frame")

test_that("frscored_cna works", {
  expect_known_hash(frscored_cna(d), hash = "ced41193da")
  expect_known_hash(frscored_cna(d, test.model = "(b+c<->A)*(e<->D)"),
                    hash = "bc36dd53d7")
  expect_error(frscored_cna(d, test.model = "(A<->D)"))
})
