#' giftRegions
#'
#' Gets regions listed by GIFT for plant species
#'
#' @importFrom jsonlite read_json 
#' @importFrom geojsonio geojson_read
#' @importFrom maptools spRbind
#' @importFrom sp spChFIDs
#' @param species character, species binomial name
#' @param min_size numeric, minimum size of checklists (in km2) to be included 
#' in the analysis.
#' @param max_size numeric, maximum size of checklists (in km2) to be included 
#' in the analysis.
#' @return This function returns a list containing three shapefiles derived by 
#' information supplied by GIFT. "regs" includes all the features corresponding
#' to regions where the species has been listed as present. "regs_native" 
#' includes all the features corresponding to regions where the species has 
#' been listed as native. And "regs_alien" includes all the features 
#' corresponding to regions where the species has been listed as alien.
#' @examples
#' gift_reference_regions <- giftRegions("Babiana tubulosa")
#' @export
giftRegions <- function(species,min_size=1000,max_size=100000000000){
  
  genus <- gsub("(^.*) (.*$)","\\1",species)  #get genus
  epithet <- gsub("(^.*) (.*$)","\\2",species)  #get epithet
  
  
  jdata <- try(suppressWarnings(jsonlite::read_json(
    paste("http://gift.uni-goettingen.de/api/?query=species_distr&genus=",
                           genus,"&epithet=",epithet,sep=""),
                     simplifyVector = TRUE)),
              silent = TRUE)
  
  if(inherits(jdata, "try-error")){
    
    message("GIFT database currently not accessible due to server issues.
         Please try again later.")
    return(NULL)
    
  }else{
    if(length(jdata)==0){warning(paste0("GIFT does not have checklists for ",
                                        species,"."))
    }else{
      for(i in seq_len(nrow(jdata)))
      {
        a <- try(suppressWarnings(geojsonio::geojson_read(
          paste("http://gift.uni-goettingen.de/geojson/geojson_smaller/", 
                #download the shapefile for each region
                jdata$entity_ID[[i]],".geojson",sep=""), what = "sp")),
          silent = TRUE)

        if(!inherits(a, "try-error")){
          a$native <- jdata$native[[i]]
          a$naturalised <- jdata$naturalized[[i]]
          b <- sp::spChFIDs(a,paste(i))
          if(i == 1){
            regs <- b
          }else{
            if(ncol(regs) == ncol(b)){
              regs <- spRbind(regs,b)
            }
          }
        }
      }
      regs <- regs[which(regs$area>min_size),]
      regs <- regs[which(regs$area<max_size),]
      regs_native <- regs[which(regs$native==1),]
      regs_alien <- regs[which(regs$native==0),]
      regs_list <- list(regs,regs_native,regs_alien)
      names(regs_list) <- c("Presence","Native","Alien")
      return(regs_list)
    }
  }
}
