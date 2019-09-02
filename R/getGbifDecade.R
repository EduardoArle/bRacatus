#' Download gbif data per decade
#'
#' Downloads gbif records decade by decade
#'
#' @import rgbif
#' @import data.table
#' @param  species character, species binomial name
#' @return The function downloads gbif records decade by decade for a given species in a given period of time. It is a secondary function to cope with the limitation of 200,000 records that can be downloaded from a single request through rgbif package. This function is composed of two parts: 1 downloads all occurrences for a species from 1001 to 1900, 2 loops thought decades from 1901 to present day
#' @export
getGbifDecade <- function(species){
  pts_1001_1900 <- occ_search(scientificName=species,  #download gbif records per period
                              limit=200000,year='1001,1900')
  if(nrow(pts_1001_1900[[3]])==200000){     #flag possible periods with more than 200000 records
    warning("There are more than 200,000 records available between 1001 and 1009. Not all have been downloaded.")
  }
  pts_decade <- list()
  for(k in 1:ceiling(((as.numeric(format(Sys.time(),"%Y")))-1900)/10))
  {
    pts_decade[[k]] <- occ_search(scientificName=species,  #download gbif records per period
                                  limit=200000,year=paste(1891+10*k,",",1890+10*k+10,sep=""),
                                  hasCoordinate=T)
    if(!is.null(nrow(pts_decade[[k]][[3]]))){
      if(nrow(pts_decade[[k]][[3]])==200000){
        pts_decade[[k]][[3]] <- downloadGbifYear(species,1891+10*k,1890+10*k+10)
      }
    }
  }
  gbif_rec[[3]] <- rbind(pts_1001_1900[[3]],rbindlist(lapply(pts_decade,function(x){x[[3]]}),fill=T),fill=T)
}