#' accuracy
#'
#' Estimates the accuracy of each point record.
#'
#' @importFrom stats predict
#' @param signals output of the function signalCalculation. A dataFrame including the original point data and the signals sent by the reference regions.
#' @return The dataFrame with the species occurrence information and an extra column indicating the estimated accuracy of each point.
#'
#' @export
accuracy <- function(signals){
  model <- bRacatus::Model_accuracy
  acc <- predict(model,signals,type="response")
  tab <- signals[,-((ncol(signals)-3):ncol(signals))]
  tab <- cbind(tab,accuracy=acc)
  return(tab)
}
