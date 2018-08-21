context("test-chronotrigger")
library(geekr)

test_that("no errors works", {
  expect_warning(
    ChronoTrigger(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
})

test_that("https/http", {
  expect_warning(
    ChronoTrigger(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  expect_warning(
    ChronoTrigger(
      sound = "http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Tried but could not download http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
  )
})

test_that("sound from sounds works", {
  expect_equal(
    ChronoTrigger(
      sound = "save"
    )
    , ChronoTrigger()
  )
})
