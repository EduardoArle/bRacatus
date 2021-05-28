#' accuracy
#'
#' Estimates the Accuracy of Each Point Record.
#'
#' @importFrom stats predict
#' @param signals output of the function "signalCalculation". A data.frame 
#' including the original point data and the signals sent by the reference 
#' regions.
#' @return The data.frame with the species occurrence information and an extra 
#' column indicating the estimated accuracy of each point.
#'
#' @export
accuracy <- function(signals) {
  signals <- as.data.frame(signals)
  model <- bRacatus::Model_accuracy
  acc <- predict(model, signals, type = "response")
  drop <- c("ID_points",names(signals)[grep("signal",names(signals))])
  tab <- signals[, !(names(signals) %in% drop)]
  tab <- cbind(tab, accuracy = acc)
  return(tab)
}