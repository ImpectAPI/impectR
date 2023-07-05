test_that("getIterations", {
  # get actual data
  actual_df <- getIterations(
    token = getAccessToken(
      username = "florian.schmitt@impect.com",
      password = "L@@A5r8FnSkqQc@"
    )
  )

  # load expected data
  expected_df <- readRDS("D:/Impect/impectR/tests/testthat/snaps/iterations.rds")

  # run test
  expect_equal(actual_df, expected_df)
})
