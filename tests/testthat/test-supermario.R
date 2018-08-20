context("test-supermario")
library(geekr)

test_that("no errors works", {
  expect_silent(SuperMario())
})
