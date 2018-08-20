context("test-chronotrigger")
library(geekr)

test_that("no errors works", {
  expect_silent(ChronoTrigger())
})
