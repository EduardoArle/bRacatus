#' occID
#'
#' Extracts cellIDs of presence locations
#'
#' @importFrom raster extract
#' @param occ_sp spatialPointsDataTable of the species occurrence.
#' @return A dataFrame including the original data in the input and the cellID of each point record
#' @examples
#' occ_sp <- bRacatus::H_mirandae_sp
#' occ_ID <- occID(occ_sp)
#'
#' @export
occID <- function(occ_sp){
  ID <- bRacatus::ID_raster
  ID_points <- extract(ID_raster,occ_sp)
  pointsID <- cbind(occ_sp@data,ID_points=ID_points)
  return(pointsID)
}
