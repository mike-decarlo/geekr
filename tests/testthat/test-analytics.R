context("test-analytics")
library(geekr)

test_that("no errors works", {
  expect_warning(
    Analytics(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})
