context("test-analytics")
library(geekr)

test_that("no errors works", {
  expect_warning(
    Analytics(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})

test_that("https returns warning", {
  expect_warning(
    Analytics(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
      )
    , "Can't currently use https urls, only http."
  )
})
