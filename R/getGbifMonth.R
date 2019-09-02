#' Download gbif data per month
#'
#' Downloads gbif records month by month
#'
#' @import rgbif
#' @import data.table
#' @param  species character, species binomial name
#' @param  year numeric, the year
#' @return The function downloads gbif records month by month for a given species in a given year. It is a secondary function to cope with the limitation of 200,000 records that can be downloaded from a single request through rgbif package.
#' @export
getGbifMonth <- function(species,year){
  #this function loops though each month downloading GBIF records for a species for a given year
  pts_month <- list()
  for(m in 1:12)
  {
    pts_month[[m]] <- rgbif::occ_search(scientificName=species,  #download gbif records per year
                                        limit=200000,year=year,month=m,
                                        hasCoordinate=T)
    if(!is.null(nrow(pts_month[[m]][[3]]))){
      if(nrow(pts_month[[m]][[3]])==200000){
        warning(paste0("There are more than 200,000 occurrences for the month ",m,"/",year,". Not all have been downloaded"))
      }
    }
  }
  pts_month2 <- rbindlist(lapply(pts_month,function(x){x[[3]]}),fill=T)
  return(pts_month2)
}