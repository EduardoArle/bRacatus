#' getOcc
#'
#' Downloads gbif records iterating when necessary to overcome the limitation of 200,000 records
#'
#' @importFrom rgbif occ_search
#' @importFrom data.table as.data.table
#' @param  species character, species binomial name
#' @return This function downloads all records for a species from GBIF that have coordinates info. If necessary it loops several times to overcome the limit of 200,000 occurrences inposed by occ_search function. It returns a data table.
#' @examples
#' sps_occurrence <- getOcc("Hemitriccus mirandae")
#' @export
getOcc <- function(species){
  gbif_rec <- occ_search(scientificName=species,limit=200000,hasCoordinate=T) #download only points with coordinates
  if(!is.null(nrow(gbif_rec[[3]]))){
    if(nrow(gbif_rec[[3]])==200000){    #if there are more than 200,000 records, it's necessary to download per parts
      gbif_rec[[3]] <- getGbifDecade(species)
    }
  }
  gbif <- as.data.table(gbif_rec[[3]])
  return(gbif)
}