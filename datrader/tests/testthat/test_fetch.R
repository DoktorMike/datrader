context("Data presence")
library(datrader)

fbpath <- system.file("extdata", "FB.csv", package = "datrader", mustWork = TRUE)
mspath <- system.file("extdata", "MSFT.csv", package = "datrader", mustWork = TRUE)
nfpath <- system.file("extdata", "NFLX.csv", package = "datrader", mustWork = TRUE)

test_that("installed data is available", {
  expect_failure(expect_equal(fbpath, ""))
  expect_failure(expect_equal(mspath, ""))
  expect_failure(expect_equal(nfpath, ""))
})
