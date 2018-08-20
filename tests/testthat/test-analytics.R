context("test-analytics")
library(geekr)

test_that("no errors works", {
  expect_silent(Analytics())
})
