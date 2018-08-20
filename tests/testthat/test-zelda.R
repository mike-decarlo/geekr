context("test-zelda")
library(geekr)

test_that("no errors works", {
  expect_silent(Zelda())
})
