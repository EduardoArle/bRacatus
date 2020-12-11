#' @importFrom testthat test_that expect_equal expect_true
#' 
context("bRacatus")

input_data <- giftRegions ("Babiana tubulosa")
input_data2 <- getOcc ("Babiana tubulosa")

test_that("Expected data structure",{
  expect_equal(class(input_data),"list")
  expect_true("Presence" %in% names(input_data))
  expect_true("Native" %in% names(input_data))
  expect_true("Alien" %in% names(input_data))
  expect_equal(class(input_data2),"data.frame")
})
