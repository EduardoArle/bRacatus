#' occSpatialPoints
#'
#' Downloads gbif records iterating when necessary to overcome the limitation 
#' of 200,000 records
#'
#' @importFrom sp proj4string
#' @importFrom sp coordinates
#' @importFrom sp CRS
#' @param occ table
#' @return This function creates spatialPoints from tables containing 
#' coordinates.
#' @examples
#' # Create a data.frame containing species names and coordinates
#'
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'              lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'              lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'              gender=rep("female",10),head_size=rep("headless individual"))
#'
#' sps_occurrence <- giveOcc(test_data,"sps","lon","lat")
#' 
#' sps_sp <- occSpatialPoints(sps_occurrence)
#' @export
occSpatialPoints <- function(occ) {
#create spatial points from GBIF coordinates
sp::coordinates(occ) <- ~decimalLongitude + decimalLatitude
sp::proj4string(occ) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
return(occ)
}
