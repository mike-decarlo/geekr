context("test-chronotrigger")
library(geekr)

test_that("no errors works", {
  expect_warning(
    ChronoTrigger(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
  Sys.sleep(5)
})

test_that("https/http", {
  expect_warning(
    ChronoTrigger(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  Sys.sleep(5)
  expect_equal(
    ChronoTrigger(
      sound = "http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , SuperMario(
      sound = "http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
  )
  Sys.sleep(5)
})

test_that("sound from sounds works", {
  expect_equal(
    ChronoTrigger(
      sound = "save"
    )
    , ChronoTrigger()
  )
})
