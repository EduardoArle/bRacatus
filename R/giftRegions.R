#' giftRegions
#'
#' Gets regions listed by GIFT for plant species
#'
#' @importFrom jsonlite read_json
#' @importFrom geojsonio geojson_read
#' @importFrom maptools spRbind
#' @param  species character, species binomial name
#' @return This function returns a list containing three shapefiles derived by information supplied by GIFT. "regs" includes all the features corresponding to regions where the species has been listed as present. "regs_native" includes all the features corresponding to regions where the species has been listed as native. And "regs_alien" includes all the features corresponding to regions where the species has been listed as alien.
#' @examples
#' gift_reference_regions <- giftRegions("Boreava aptera")
#' @export
giftRegions <- function(species){
  
  `%ni%` <- Negate(`%in%`) #create operator to do "not in" 
  genus <- gsub("(^.*) (.*$)","\\1",species)  #get genus
  epithet <- gsub("(^.*) (.*$)","\\2",species)  #get epithet
  

  jdata <- read_json(paste("http://gift.uni-goettingen.de/api/?query=species_distr&genus=",
                           genus,"&epithet=",epithet,sep=""),
                     simplifyVector = TRUE)  
  
  for(i in 1:nrow(jdata)) #some times: Error in spRbind(as(obj, "SpatialPolygons"), as(x, "SpatialPolygons")) : non-unique polygon IDs
  {
    a <- geojson_read(paste("http://gift.uni-goettingen.de/geojson/geojson_smaller/", #download the shapefile for each region
                            jdata$entity_ID[[i]],".geojson",sep=""), what = "sp")
    a$native <- jdata$native[[i]]
    a$naturalised <- jdata$naturalized[[i]]
    if(i==1){
      regs <- a
    }else{
      if(a$entity_ID %ni% regs$entity_ID){
        regs <- spRbind(regs,a)
      }
    }
  }
  regs_native <- regs[which(regs$native==1),]
  regs_alien <- regs[which(regs$native==0),]
  regs_list <- list(regs,regs_native,regs_alien)
  names(regs_list) <- c("Presence","Native","Alien")
  return(regs_list)
}

