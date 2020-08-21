library(testthat)
library(checkmate)
context("bRacatus")

input_data <- "Hemitriccus mirandae"
  
test_that("Expected data structure",{
  expect_equal(class(input_data),"character")
})
