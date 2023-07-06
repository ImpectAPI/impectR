test_that("getMatchplan", {
  # get actual data
  actual_df <- getMatchplan(
    iteration = 518,
    token = getAccessToken(
      username = "florian.schmitt@impect.com",
      password = "L@@A5r8FnSkqQc@"
    )
  )

  # load expected data
  expected_df <- readRDS("D:/Impect/impectR/tests/testthat/snaps/matchplan.rds")

  # run test
  expect_equal(actual_df, expected_df)
})
