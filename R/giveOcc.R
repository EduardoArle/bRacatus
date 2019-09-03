#' Input occurrence data
#'
#' Prepares user privided georeferrenced biological data for the models
#'
#' @import data.table
#' @param  occ_data table containing latitude and longitude
#' @param  species col.name containing the species information
#' @param  longitude col.name containing the longitude information
#' @param  latitude col.name containing the latitude information
#' @return This function standardises the user provided georeferreced biological data to be fed into the models.
#' @export
giveOcc <- function(table,species="species",longitude="longitude",latitude="latitude"){
  table2 <- table
  names(table2)[which(names(table2)==species)] <- "species"
  names(table2)[which(names(table2)==longitude)] <- "decimalLongitude"
  names(table2)[which(names(table2)==latitude)] <- "decimalLatitude"
  return(table2)
}
