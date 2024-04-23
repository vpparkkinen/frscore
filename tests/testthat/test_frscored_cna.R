
local_edition(3)

d <- structure(list(A = c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L),
               B = c(1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L),
               C = c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L),
               D = c(0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L),
               E = c(1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L)),
          row.names = c("10", "13", "15", "18", "19", "22", "23", "27"),
          class = "data.frame")

d_sets <- list(d.error, d.autonomy, d.pban)

fr <- expand.grid(seq(0.5, 1, by = 0.1),
                  seq(0.5, 1, by = 0.1),
                  c(0.05, 0.1),
                  c("csf", "asf", "msc"),
                  seq(1, 200, by = 20),
                  seq_along(d_sets)
                  )

test_that("frscore_cna returns when it should",{
  skip_on_cran()
  skip_on_ci()
  for (i in 1:nrow(fr)) {
    expect_no_error(suppressWarnings(
      frscored_cna(d_sets[[fr[i, 6]]],
                   fit.range = c(fr[i, 1], fr[i, 2]),
                   granularity = fr[i, 3],
                   output = as.character(fr[i, 4]),
                   maxsols = fr[i, 5],
                   outcome = if (fr[i, 6] != 3) {
                     sample(names(d_sets[[fr[i, 6]]]), 1)
                   } else {paste0(sample(names(d.pban), 1),
                                  "=",
                                  "1")}
                   )

      )
    )
  }
})



test_that("frscored_cna works", {
  expect_snapshot(frscored_cna(d.error))
  expect_snapshot(frscored_cna(d.error, normalize = "idealmax"))
  expect_snapshot(frscored_cna(d.error, normalize = "none"))
  expect_snapshot(frscored_cna(d.error, verbose = TRUE))

  expect_snapshot(frscored_cna(d.pban))
  expect_snapshot(frscored_cna(d.jobsecurity,
                               fit.range = c(0.8, 0.7),
                               granularity = 0.1,
                               outcome = "JSR"))
})


test_that("frscored_cna fails when it should", {
  expect_error(frscored_cna(d, test.model = "(A<->D)"))
})
