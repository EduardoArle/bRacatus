#' Download gbif data per year
#'
#' Downloads gbif records year by year
#'
#' @import rgbif
#' @import data.table
#' @param  species character, species binomial name
#' @param  year_0 numeric, the first year of the period
#' @param  year_1 numeric, the last year of the period
#' @return The function downloads gbif records year by year for a given species in a given period of time. It is a secondary function to cope with the limitation of 200,000 records that can be downloaded from a single request through rgbif package.
#' @export
getGbifYear <- function(species,year_0,year_1){
  #this function loops though year_0 to year_1 downloading all GBIF records for a species
  pts_year <- list()
  for(l in 1:(year_1-year_0+1))
  {
    pts_year[[l]] <- rgbif::occ_search(scientificName=species,  #download gbif records per year
                                limit=200000,year=paste(year_0-1+l),
                                hasCoordinate=T)
    if(!is.null(nrow(pts_year[[l]][[3]]))){
      if(nrow(pts_year[[l]][[3]])==200000){
        pts_year[[l]][[3]] <- getGbifMonth(species,year_0-1+l)
      }
    }
  }
  pts2 <- rbindlist(lapply(pts_year,function(x){x[[3]]}),fill=T)
  return(pts2)
}