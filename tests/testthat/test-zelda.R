context("test-zelda")
library(geekr)

test_that("no errors works", {
  expect_warning(
    zelda(sound = "F")
    , "'F' is not a valid sound nor path, playing a random sound instead."
  )
  Sys.sleep(5)
})

test_that("https/http", {
  expect_warning(
    zelda(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  Sys.sleep(5)
  expect_warning(
    zelda(sound = "http://amazonsmile.com")
  )
})

test_that("sound from sounds works", {
  expect_equal(
    zelda(
      sound = "fanfare"
    )
    , zelda()
  )
})
