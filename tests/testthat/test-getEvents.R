test_that("getEvents", {
  # get actual data
  actual_df <- getEvents(
    matches = c(84248),
    token = getAccessToken(
      username = "florian.schmitt@impect.com",
      password = "L@@A5r8FnSkqQc@"
    )
  )

  # load expected data
  expected_df <- readRDS("D:/Impect/impectR/tests/testthat/snaps/events.rds")

  # run test
  expect_equal(actual_df, expected_df)
})
