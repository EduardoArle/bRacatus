#' occSpatialPoints
#'
#' Downloads gbif records iterating when necessary to overcome the limitation of 200,000 records
#'
#' @import raster
#' @import rgdal
#' @import sp
#' @param  occ table
#' @return This function creates spatialpoints from tables containing coordinates.
#' @examples
#' # Create a data.frame containing species names and coordinates
#'
#' sps_occurrence <- getOcc("Hemitriccus mirandae")
#' sps_sp <- occSpatialPoints(sps_occurrence)
#' @export
occSpatialPoints <- function(occ){
  occ_sp <- occ  #create spatial points from GBIF coordinates
  sp::coordinates(occ_sp) <- ~decimalLongitude+decimalLatitude
  sp::proj4string(occ_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(occ_sp)
}
