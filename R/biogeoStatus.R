#' biogeoStatus
#'
#' Estimates the biogeographic status of each point record.
#'
#' @importFrom stats predict
#' @param signals output of the function signalCalculation. A dataFrame including the original point data and the signals sent by the reference regions.
#' @return The dataFrame with the species occurrence information and an extra column indication the estimated biogeographic status of each point.
#'
#' @export
biogeoStatus <- function(signals){
  model <- bRacatus::Model_biogeo
  biogeo <- predict(model,signals,type="response")
  tab <- signals[,-c((ncol(signals)-3):ncol(signals))]
  tab <- cbind(tab,biogeoStatus=biogeo)
  return(tab)
}
