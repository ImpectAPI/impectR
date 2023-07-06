test_that("getSquadIterationAverages", {
  # get actual data
  actual_df <- getSquadIterationAverages(
    iteration = 518,
    token = getAccessToken(
      username = "florian.schmitt@impect.com",
      password = "L@@A5r8FnSkqQc@"
    )
  )

  # load expected data
  expected_df <- readRDS("D:/Impect/impectR/tests/testthat/snaps/squadIterationAverages.rds")

  # run test
  expect_equal(actual_df, expected_df)
})
