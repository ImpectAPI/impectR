test_that("getPlayerMatchsums", {
  # get actual data
  actual_df <- getPlayerMatchsums(
    matches = c(84248),
    token = getAccessToken(
      username = "florian.schmitt@impect.com",
      password = "L@@A5r8FnSkqQc@"
    )
  )

  # load expected data
  expected_df <- readRDS("D:/Impect/impectR/tests/testthat/snaps/playerMatchsums.rds")

  # run test
  expect_equal(actual_df, expected_df)
})
