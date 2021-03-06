
local_edition(2)

d <- structure(list(A = c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L),
               B = c(1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L),
               C = c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L),
               D = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L),
               E = c(1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)),
          row.names = c("10", "13", "15", "18", "19", "22", "23", "27"),
          class = "data.frame")

test_that("frscored_cna works", {
  expect_output(str(frscored_cna(d.error)), "List of 10")
})


test_that("frscored_cna fails when it should", {
  expect_error(frscored_cna(d, test.model = "(A<->D)"))
})
