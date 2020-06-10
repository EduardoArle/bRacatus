#' plotAccuracy
#'
#' Plot the species occurrences showing the estimated accuracy of points.
#'
#' @importFrom graphics points
#' @importFrom plotfunctions gradientLegend
#' @importFrom raster extent
#' @importFrom raster plot
#' @importFrom rgeos gIntersection
#' @importFrom rworldmap getMap
#' @importFrom sp over
#' @importFrom sp proj4string
#' @param acc dataTable of the species occurrence including a column with the estimated accuracy of points.
#' @param regional logical, whether the whole world should be plotted as the background or only the region adjacent to the species countries of occurrence.
#' @return This function plots the species occurrence with estimated accuracy of all points.
#' @export
plotAccuracy <- function(acc,regional=TRUE){
  world <- getMap(resolution = "low")
  acc_sp <- occSpatialPoints(acc)
  if(regional){
    countries <- unique(over(acc_sp,world)$NAME)
    countries <- world[world$NAME %in% countries,]
    CP <- as(extent(countries), "SpatialPolygons")
    sp::proj4string(CP) <- CRS(proj4string(world))
    map <- suppressWarnings(gIntersection(world,CP,byid=TRUE))
  }else{
    map <- world
  }
  colacc <- colorRampPalette(c("#c51b7d","#e9a3c9","#fde0ef","#f7f7f7","#e6f5d0","#a1d76a","#4d9221"))
  col_pts_acc <- colacc(100)[cut(c(0,1,acc$accuracy),breaks = 100)]
  col_pts_acc <- col_pts_acc[-c(1,2)]
  
  par(mfrow=c(1,1),mar=c(1.5,1.5,1.5,5))
  plot(map,col="khaki",bg="azure2",main=unique(acc$species),font.main=3)
  points(acc_sp,pch=21,cex=1,bg=col_pts_acc)
  gradientLegend(valRange=c(0,1),pos=c(1.03,0.2,1.045,0.8),side=4,color=colacc(20),n.seg=3)
}
