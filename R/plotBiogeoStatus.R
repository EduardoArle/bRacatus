#' plotAccuracy
#'
#' Plot the species occurrences showing the estimated biogeographical status of points.
#'
#' @importFrom graphics points
#' @importFrom raster extent
#' @importFrom raster plot
#' @importFrom rgeos gIntersection
#' @importFrom rworldmap getMap
#' @importFrom sp over
#' @importFrom sp proj4string
#' @param acc dataTable of the species occurrence including a column with the estimated biogeographical status of points.
#' @param regional logical, whether the whole world should be plotted as the background or only the region adjacent to the species countries of occurrence.
#' @return This function plots the species occurrence with estimated biogeographical status of all points.
#' @export
plotBiogeoStatus <- function(biogeo,regional=TRUE){
  world <- getMap(resolution = "low")
  biogeo_sp <- occSpatialPoints(biogeo)
  if(regional){
    countries <- unique(over(biogeo_sp,world)$NAME)
    countries <- world[world$NAME %in% countries,]
    CP <- as(extent(map), "SpatialPolygons")
    proj4string(CP) <- CRS(proj4string(world))
    map <- suppressWarnings(gIntersection(world,CP,byid=TRUE))
  }else{
    map <- world
  }
  colbiogeo <- colorRampPalette(c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf","#2166ac"))
  col_pts_biogeo <- colbiogeo(100)[cut(c(0,1,biogeo$biogeoStatus),breaks = 100)]
  col_pts_biogeo <- col_pts_biogeo[-c(1,2)]
  
  par(mfrow=c(1,1),mar=c(1.5,1.5,1.5,5))
  plot(map,col="khaki",bg="azure2",main=unique(biogeo$species),font.main=3)
  points(biogeo_sp,pch=21,cex=1,bg=col_pts_biogeo)
  gradientLegend(valRange=c(0,1),pos=c(1.03,0.2,1.045,0.8),side=4,color=colbiogeo(20),n.seg=3)
}
