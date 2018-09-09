context("Data loading")
library(datrader)

fbpath <- system.file("extdata", "FB.csv", package = "datrader", mustWork = TRUE)
mspath <- system.file("extdata", "MSFT.csv", package = "datrader", mustWork = TRUE)
nfpath <- system.file("extdata", "NFLX.csv", package = "datrader", mustWork = TRUE)


fb4lines <-
  structure(
    list(
      Index = structure(c(15478, 15481, 15482, 15483), class = "Date"),
      open = c(42.049999, 36.529999, 32.610001, 31.370001),
      high = c(45, 36.66, 33.59, 32.5),
      low = c(38, 33, 30.940001, 31.360001),
      close = c(38.23, 34.029999, 31, 32),
      volume = c(573576400L, 168192700L, 101786600L, 73600000L),
      adj. = c(38.23, 34.029999, 31, 32)
    ),
    .Names = c("Index", "open", "high", "low", "close", "volume", "adj."),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
  )

test_that("installed data is correct", {
  expect_true(all(head(readInstrument(fbpath), 4)==fb4lines))
})

instlist <- loadExistingInstruments(gsub("/FB.csv", "", fbpath))

test_that("multiple data can be loaded", {
  expect_true(setequal(c("FB", "MSFT", "NFLX"), names(instlist)))
})

test_that("last date is correct", {
  expect_true(findLastDateInInstruments(gsub("/FB.csv", "", fbpath)) == "2018-09-07")
})
