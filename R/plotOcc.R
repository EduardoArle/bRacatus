#' plotOcc
#'
#' Plot the species occurrences with map background for visualisation
#'
#' @importFrom graphics points
#' @importFrom raster extent plot
#' @importFrom rgeos gIntersection gBuffer
#' @importFrom rworldmap getMap
#' @importFrom sp over proj4string
#' @param occ dataTable of the species occurrence.
#' @param regional logical, whether the whole world should be plotted as the 
#' background or only the region adjacent to the species countries of 
#' occurrence.
#' @return This function plots the species occurrence
#' @examples
#' 
#' \donttest{
#' occ <- getOcc("Hemitriccus mirandae")
#' 
#' plotOcc(occ)
#' 
#' test_data <- data.frame(sps=rep("Equus acephalus",10),
#'              lon=c(-43.2,-58.4,-56,-44,-54.5,-57.4,-60.1,-68.5,-71.3,-47.5),
#'              lat=c(-22.9,-34.6,-34.8,-20,-25.5,-25.2,-3,-32.5,-41.1,-15.5),
#'              gender=rep("female",10),head_size=rep("headless individual"))
#'
#' occ <- giveOcc(test_data,"sps","lon","lat")
#' 
#' plotOcc(occ)
#' # Plot occurrences with the whole world as background
#' 
#' plotOcc(occ,regional=FALSE)
#' }
#' 
#' @export
plotOcc <- function(occ, regional = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  occ_sp <- occSpatialPoints(occ)
  if(regional == TRUE){
    countries <- unique(over(occ_sp,world)$NAME)
    countries <- world[world$NAME %in% countries,]
    CP <- as(extent(countries), "SpatialPolygons")
    sp::proj4string(CP) <- CRS(proj4string(world))
    map <- suppressWarnings(gIntersection(world,
                                          CP,
                                          byid = TRUE, 
                                          checkValidity = 2))
  } else {
    map <- world
  }
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  plot(map, col = "khaki", bg = "azure2",
       main = unique(occ_sp$species), font.main = 3)
  points(occ_sp, pch = 21, cex = 1, bg = "red")
}
