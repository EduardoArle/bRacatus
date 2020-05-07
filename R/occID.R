#' occID
#'
#' Extracts cellIDs of presence locations
#'
#' @import raster
#' @import rgdal
#' @param  occ_sp spatialPointsDataTable of the species occurrence.
#' @return A spatialPointsDataTable including the cellID of each point record
#' @examples
#' country_checklist <- countryChecklist(c("Brazil","Argentina","Uruguay","Paraguay"),
#' c("native","alien","unknown","native"))
#'
#' rasterised_checklist <- rasteriseChecklists(country_checklist)
#' 
#' value_IDs <- valueID(rasterised_checklist)
#' @export
occID <- function(occ_sp){
  # ID_prob <- list()
  
  ID <- bRacatus::ID_raster
  # for(i in 1:length(checklists_raster))
  # {
  #   cell_ID <- which(checklists_raster[[i]][]!=0)
  #   prob <- checklists_raster[[i]][which(checklists_raster[[i]][]!=0)]
  #   ID_prob[[i]] <- data.frame(cell_ID=cell_ID,prob=prob)
  # }
  # names(ID_prob) <- names(checklists_raster)
  return(ID)
}



# gbifCellID <- function(gbif,gbif_sp){
#   library(raster);library(rgdal)
#   setwd("C:/Users/ca13kute/Documents/bRacatus/Package") #load raster with ID_values for land
#   ID_raster <- raster("ID_raster.img")
#   ID_points <- extract(ID_raster,gbif_sp)
#   gbifID <- cbind(gbif,ID_points)
#   return(gbifID)
# }

# # setwd("C:/Users/ca13kute/Documents/bRacatus/Package/bRacatus/data")
# 
# save(ID_raster,file="ID_raster.RData")
# 
# ?save

