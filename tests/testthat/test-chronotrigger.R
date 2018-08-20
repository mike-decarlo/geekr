context("test-chronotrigger")
library(geekr)

test_that("no errors works", {
  expect_warning(
    ChronoTrigger(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})
