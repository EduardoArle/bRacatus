#' valueID
#'
#' Extracts signal values and ID from each cell
#'
#' @param  checklists_raster List containing rasterised checklists for 
#' presence, native and alien reference regions
#' @return A list with cell IDs and signal values for all cells where the 
#' species is present, native and alien.
#' @examples
#' country_checklist <- countryChecklist(
#' c("Brazil","Argentina","Uruguay","Paraguay"),
#' c("native","alien","unknown","native"))
#'
#' rasterised_checklist <- rasteriseChecklists(country_checklist)
#' 
#' value_IDs <- valueID(rasterised_checklist)
#' @export
valueID <- function(checklists_raster) {
  ID_prob <- list()
  for (i in seq_along(checklists_raster)) {
    cell_ID <- which(checklists_raster[[i]][] != 0)
    prob <- checklists_raster[[i]][which(checklists_raster[[i]][] != 0)]
    ID_prob[[i]] <- data.frame(cell_ID = cell_ID, prob = prob)
  }
  names(ID_prob) <- names(checklists_raster)
  return(ID_prob)
}