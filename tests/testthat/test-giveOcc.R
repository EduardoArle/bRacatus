#' @importFrom testthat test_that expect_equal expect_true
#' 
context("bRacatus")

input_data <-  data.frame(sps=rep("Equus acephalus",10),
                lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
                lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
                gender=rep("female",10),head_size=rep("headless individual"))

test_that("Expected data structure",{
  expect_equal(class(input_data),"data.frame")
  expect_true("sps" %in% names(input_data))
  expect_true("lat" %in% names(input_data))
  expect_true("lon" %in% names(input_data))
  expect_equal(class(input_data$lat),"numeric")
  expect_equal(class(input_data$lon),"numeric")
})
