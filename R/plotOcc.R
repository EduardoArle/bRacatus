#' plotOcc
#'
#' Plot the species occurrences with map background for visualisation
#'
#' @importFrom graphics points
#' @importFrom rworldmap getMap
#' @param  occ_sp spatialPointsDataTable of the species occurrence.
#' @param  regional logical, whether the whole world should be plotted as the background or only the region adjacent to the species countries of occurrence.
#' @return This function plots the species occurrence
#' @examples
#' occ <- getOcc("Hemitriccus mirandae")
#' occ_sp <- occSpatialPoints(occ)
#' 
#' plotOcc(occ_sp)
#' 
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'                        lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'                        lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'                        gender=rep("female",10),head_size=rep("headless individual"))
#'
#' occ <- giveOcc(test_data,"sps","lon","lat")
#' occ_sp <- occSpatialPoints(occ)
#' 
#' plotOcc(occ_sp)
#' 
#' \dontrun{
#' # Plot occurrences with the whole world as background
#' 
#' plotOcc(occ_sp,regional=FALSE)
#' }
#' 
#' @export
plotOcc <- function(occ_sp,regional=TRUE){
  world <- getMap()
  if(regional==T){
    countries <- unique(over(occ_sp,world)$NAME)
    map <- world[world$NAME %in% countries,]
  }else{
    map <- world
  }
  plot(map,col="khaki",bg="azure2",main=unique(occ_sp$species))
  points(occ_sp,pch=19,cex=0.6,col="red")
}