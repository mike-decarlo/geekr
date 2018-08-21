context("test-analytics")
library(geekr)

test_that("no errors works", {
  expect_warning(
    Analytics(sound = "F")
    , '"F" is not a valid sound nor path, playing a random sound instead.'
  )
  Sys.sleep(5)
})

test_that("https/http", {
  expect_warning(
    Analytics(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  Sys.sleep(5)
  expect_warning(
    Analytics(
      sound = "http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Tried but could not download http://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
  )
  Sys.sleep(5)
})

test_that("sound from sounds works", {
  expect_equal(
    Analytics(
      sound = "all_done"
    )
    , Analytics()
  )
})
