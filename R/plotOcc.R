#' plotOcc
#'
#' Plot the species occurrences with map background for visualisation
#'
#' @importFrom graphics points
#' @importFrom raster crs extent plot
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_intersects st_intersection st_transform 
#' @importFrom sf st_polygon st_sfc
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
#' 
#' # Plot occurrences with the whole world as background
#' 
#' plotOcc(occ,regional=FALSE)
#' }
#' 
#' @export
plotOcc <- function(occ, regional = TRUE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- ne_countries(returnclass = "sf", scale = 'large')
  old_proj <- crs(world)
  world <- st_transform(world, crs = 3857)
  
  occ_sf <- st_as_sf(occ, coords = c('decimalLongitude', 'decimalLatitude'),
                     crs = old_proj)
  occ_sf <- st_transform(occ_sf, crs = 3857)
  
  if(regional){

    countries <- unique(vapply(st_intersects(occ_sf,world), 
                          function(x) if (length(x)==0) NA_integer_ else x[1],
                          FUN.VALUE = 1))
    
    if(length(which(is.na(countries))) == 1){
      countries <- countries[-which(is.na(countries))]
    }
    
    countries <- world[countries,]
    
    coords_CP <- extent(countries)
    
    CP <- st_polygon(list(cbind(
      c(coords_CP[1],coords_CP[2],coords_CP[2],coords_CP[1],coords_CP[1]),
      c(coords_CP[3],coords_CP[3],coords_CP[4],coords_CP[4],coords_CP[3]))))
    
    CP <- st_sfc(CP, crs = crs(world))
    CP <- st_as_sf(CP)
    
    map <- suppressWarnings(st_intersection(world, CP))
    
  } else {
    
    map <- world
    
  }
  
  map <- st_transform(map, crs = old_proj)
  occ_sf <- st_transform(occ_sf, crs = old_proj)
  
  par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
  plot(st_geometry(map), col = "khaki", bg = "azure2",
       main = unique(occ_sf$species), font.main = 3)
  
  plot(st_geometry(occ_sf), add = T, pch = 21, cex = 1, bg = 'red') 
}
