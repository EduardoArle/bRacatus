#' rangeMaps
#'
#' Prepares range maps input by the user to be used as reference regions
#'
#' @importFrom raster raster
#' @importFrom raster projectRaster
#' @importFrom raster crop
#' @importFrom raster rasterize
#' @importFrom raster rasterToPolygons
#' @importFrom raster area
#' @param  range SpatialPolygonsDataFrame
#' @param  biogeo character, name of the column containing information on 
#' biogeographic status of features
#' @param  native character, entries in biogeo column representing the native 
#' range of the species
#' @param  alien character, entries in biogeo column representing the alien 
#' range of the species
#' @return This function returns a list containing three shapefiles derived 
#' from information supplied by the species range map in a shapefile format. 
#' "regs" includes all the features corresponding to regions where the species 
#' has been listed as present. "regs_native" includes all the features 
#' corresponding to regions where the species has been listed as native. 
#' And "regs_alien" includes all the features corresponding to regions where 
#' the species has been listed as alien.
#' @examples
#' range_map_reference_regions <- rangeMaps(Range_Phalanger_orientalis)
#' @export
rangeMaps <- function(range, 
                      biogeo = "legend",
                      native = "Extant (resident)",
                      alien = "Introduced") {
  
    range_native <- range[which(range@data[, biogeo] %in% native), ]
    range_alien <- range[which(range@data[, biogeo] %in% alien), ]
  
  raster_2degrees <- raster(vals = NA, res = 2)
  raster_cut <- crop(raster_2degrees, extent(range) + c(-2, 2, -2, 2))
  
  range2 <- rasterize(range, raster_cut, getCover = TRUE)
  #rasterise the range map
  #count also very small features
  range3 <- rasterToPolygons(range2)
  range4 <- range3[which(range3$layer != 0), ]
  range4$data <- 1
  range4 <- range4[which(range4$layer >= 0.01), ]
  range4$area <- raster::area(range4)/1e+06
  
  if(nrow(range_native) > 0){
    range_native2 <- rasterize(range_native, raster_cut, getCover = TRUE)  
    #rasterise the range map #count also very small features
    range_native3 <- rasterToPolygons(range_native2)
    range_native4 <- range_native3[which(range_native3$layer != 0), ]
    range_native4$data <- 1
    range_native4 <- range_native4[which(range_native4$layer >= 0.01), ]
    range_native4$area <- raster::area(range_native4)/1e+06
  }else{
    range_native4 <- range_native
  }

  if(nrow(range_alien) > 0){
    range_alien2 <- rasterize(range_alien, raster_cut, getCover = TRUE)  
    #rasterise the range map #count also very small features
    range_alien3 <- rasterToPolygons(range_alien2)
    range_alien4 <- range_alien3[which(range_alien3$layer != 0), ]
    range_alien4$data <- 1
    range_alien4 <- range_alien4[which(range_alien4$layer >= 0.01), ]
    range_alien4$area <- raster::area(range_alien4)/1e+06
  }else{
    range_alien4 <- range_alien
  }

  range_list <- list(range4, range_native4, range_alien4)
  names(range_list) <- c("Presence", "Native", "Alien")
  return(range_list)
}
