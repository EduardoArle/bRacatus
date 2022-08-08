#' biogeoStatus
#'
#' Estimates the biogeographic status of each point record.
#'
#' @importFrom stats predict
#' @param signals output of the function signalCalculation. A dataFrame 
#' including the original point data and the signals sent by the reference 
#' regions.
#' @return The dataFrame with the species occurrence information and an extra 
#' column indicating the estimated biogeographic status of each point.
#'
#' @export
biogeoStatus <- function(signals) {
  signals <- as.data.frame(signals)
  
  if(NaN %in% unique(signals$signal_native32_10)){
    signals$signal_native32_10[which(is.na(signals$signal_native32_10))] <- 0
  }
  
  if(NaN %in% unique(signals$signal_alien32_10)){
    signals$signal_alien32_10[which(is.na(signals$signal_alien32_10))] <- 0
  }
  
  model <- bRacatus::Model_biogeo
  biogeo <- predict(model, signals, type = "response")
  tab <- signals[, -((ncol(signals) - 3):ncol(signals))]
  tab <- cbind(tab, biogeoStatus = biogeo)
  return(tab)
}
