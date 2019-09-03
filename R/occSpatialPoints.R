#' Download gbif data using the function rgbif
#'
#' Downloads gbif records iterating when necessary to overcome the limitation of 200,000 records
#'
#' @import rgdal
#' @import raster
#' @param  occ table
#' @return This function creates spatialpoints from tables containing coordinates.
#' @export
occSpatialPoints <- function(occ){
  occ_sp <- occ  #create spatial points from GBIF coordinates
  coordinates(occ_sp) <- ~decimalLongitude+decimalLatitude
  proj4string(occ_sp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(occ_sp)
}