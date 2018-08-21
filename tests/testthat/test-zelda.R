context("test-zelda")
library(geekr)

test_that("no errors works", {
  expect_warning(
    Zelda(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})

test_that("https returns warning", {
  expect_warning(
    Zelda(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
})

test_that("sound from sounds works", {
  expect_silent(
    Zelda(
      sound = 1
    )
  )
})
