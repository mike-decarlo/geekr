context("test-chrono_trigger")
library(geekr)

test_that("no errors works", {
  expect_warning(
    chrono_trigger(sound = "F")
    , "'F' is not a valid sound nor path, playing a random sound instead."
  )
  Sys.sleep(5)
})

test_that("https/http", {
  expect_warning(
    chrono_trigger(
      sound = "https://themushroomkingdom.net/sounds/wav/smw/smw_1-up.wav"
    )
    , "Can't currently use https urls, only http."
  )
  Sys.sleep(5)
  expect_warning(
    chrono_trigger(sound = "http://amazonsmile.com")
  )
})

test_that("sound from sounds works", {
  expect_equal(
    chrono_trigger(
      sound = "save"
    )
    , chrono_trigger()
  )
})
