#' Input occurrence data
#'
#' Prepares user provided georeferenced biological data for the models
#'
#' @param  occ_data table containing latitude and longitude
#' @param  species col.name containing the species information
#' @param  longitude col.name containing the longitude information
#' @param  latitude col.name containing the latitude information
#' @return This function standardises the user provided georeferenced 
#' biological data to be fed into the models.
#' @examples
#' # Create a data.frame containing species names and coordinates
#'
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'              lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'              lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'              gender=rep("female",10),head_size=rep("headless individual"))
#'
#' sps_occurrence <- giveOcc(test_data,"sps","lon","lat")
#' @export
giveOcc <- function(occ_data, 
                    species = "species", 
                    longitude = "longitude", 
                    latitude = "latitude") {
  table2 <- occ_data
  names(table2)[which(names(table2) == species)] <- "species"
  names(table2)[which(names(table2) == longitude)] <- "decimalLongitude"
  names(table2)[which(names(table2) == latitude)] <- "decimalLatitude"
  return(table2)
}
