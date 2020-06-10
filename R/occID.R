#' occID
#'
#' Extracts cellIDs of presence locations
#'
#' @importFrom raster extract
#' @param occ dataTable of the species occurrence.
#' @return A dataFrame including the original data in the input and the cellID of each point record
#' @examples
#' occ <- getOcc("Hemitriccus mirandae")
#' occ_ID <- occID(occ)
#'
#' @export
occID <- function(occ){
  occ_sp <- occSpatialPoints(occ)
  ID <- raster(res=1/2)
  ID[] <- c(1:length(t))
  ID_points <- extract(ID,occ_sp)
  occ$ID_points <- ID_points
  return(occ)
}
