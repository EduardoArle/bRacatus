#' getOcc
#'
#' Downloads GBIF records iterating when necessary to overcome the limitation 
#' of 200,000 records
#'
#' @importFrom rgbif occ_search
#' @param  species character, species binomial name
#' @return This function downloads all records for a species from GBIF that 
#' have coordinates info. If necessary it loops several times to overcome the 
#' limit of 200,000 occurrences imposed by occ_search function. It returns a 
#' data table.
#' @examples
#' sps_occurrence <- getOcc("Babiana tubulosa")
#' @export
getOcc <- function(species) {
  gbif_rec <- try(suppressWarnings(occ_search(scientificName = species,
                                              limit = 2e+05, 
                                              hasCoordinate = TRUE)),
                  silent = TRUE)
  
  if(inherits(gbif_rec, "try-error")){
   
    message("GBIF database not accessible due to connection issues.
            Please try again later.")
      return(NULL)
    
  }else{
    
    if (!is.null(nrow(gbif_rec[[3]]))) {
      if (nrow(gbif_rec[[3]]) == 2e+05) {
# if there are more than 200,000 records, it's necessary to download per parts
        gbif_rec[[3]] <- getGbifDecade(species)
      }
    }
    gbif <- as.data.frame(gbif_rec[[3]])
  }
  
  return(gbif)
}
