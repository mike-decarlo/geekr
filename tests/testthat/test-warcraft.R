context("test-warcraft")
library(geekr)

test_that("no errors works", {
  expect_warning(
    Warcraft(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})
