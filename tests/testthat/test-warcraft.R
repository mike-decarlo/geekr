context("test-warcraft")
library(geekr)

test_that("no errors works", {
  expect_warning(
    warcraft(sound = "F")
    , "'F' is not a valid sound nor path, playing a random sound instead."
  )
  Sys.sleep(5)
})

test_that("https/http", {
  expect_warning(
    warcraft(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  Sys.sleep(5)
  expect_warning(
    warcraft(sound = "http://amazonsmile.com")
  )
})

test_that("sound from sounds works", {
  expect_equal(
    warcraft(
      sound = "jobsdone"
    )
    , warcraft()
  )
})
