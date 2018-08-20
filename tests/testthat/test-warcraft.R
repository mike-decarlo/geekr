context("test-warcraft")
library(geekr)

test_that("no errors works", {
  expect_silent(Warcraft())
})
