#' @importFrom testthat test_that expect_equal
#' 
context("bRacatus")

input_data <- "Boreava aptera"

test_that("Expected data structure",{
  expect_equal(class(input_data),"character")
})
