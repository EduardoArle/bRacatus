#' plotBiogeoStatus
#'
#' Plot the species occurrences showing the estimated biogeographical status 
#' of points.
#'
#' @importFrom graphics points par
#' @importFrom plotfunctions gradientLegend
#' @importFrom raster extent plot
#' @importFrom rgeos gIntersection gBuffer
#' @importFrom rworldmap getMap
#' @importFrom sp over proj4string
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom methods as
#' @param biogeo dataTable of the species occurrence including a 
#' column with the estimated biogeographical status of points.
#' @param regional logical, whether the whole world should be plotted as the 
#' background or only the region adjacent to the species countries of 
#' occurrence.
#' @param reg.by character, by countries where there are points or by area 
#' where the points are located.
#' @param borders logical, whether country limits should be plotted.
#' @param col.features colour for plotting features.
#' @param col.bg colour for plotting the background.
#' @param plot.range logical, if TRUE, range maps should be provided as 
#' a shapefile in argument range.
#' @param range shapefile, species range map.
#' @param box logical, includes frame with coordinates locations.
#' @return This function plots the species occurrence with estimated 
#' biogeographical status of all points.
#' @export
plotBiogeoStatus <- function(biogeo, regional = TRUE, reg.by = "country", 
                             borders = TRUE, col.features = "khaki", 
                             col.bg = "azure2", plot.range = FALSE, 
                             range = NULL, box = FALSE) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  world <- getMap(resolution = "low")
  world <- suppressWarnings(gBuffer(world, byid = TRUE, width = 0))
  biogeo_sp <- occSpatialPoints(biogeo)
  if(regional){
    if(reg.by=="country"){
      countries <- unique(over(biogeo_sp,world)$NAME)
      countries <- world[world$NAME %in% countries,]
      CP <- as(extent(countries), "SpatialPolygons")
      sp::proj4string(CP) <- CRS(proj4string(world))
      map <- suppressWarnings(gIntersection(world, CP, byid = TRUE, 
                                            checkValidity = 2))
    }
    if (reg.by == "points") {
      CP <- as(extent(biogeo_sp) + c(-1.5, 1.5, -1.5, 1.5), "SpatialPolygons")
      sp::proj4string(CP) <- CRS(proj4string(world))
      map <- suppressWarnings(gIntersection(world, CP, byid = TRUE, 
                                            checkValidity = 2))
    }
  } else {
    map <- world
  }
  colbiogeo <- colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#fddbc7", 
                                  "#f7f7f7", "#f7f7f7", "#f7f7f7", "#d1e5f0", 
                                  "#d1e5f0", "#67a9cf", "#2166ac"))
  col_pts_biogeo <- colbiogeo(100)[cut(c(0, 1, biogeo$biogeoStatus), 
                                       breaks = 100)]
  col_pts_biogeo <- col_pts_biogeo[-c(1, 2)]
  
  if (borders) {
    bord <- "black"
  } else {
    bord <- NA
  }
  
  par(mfrow = c(1, 1), mar = c(1.5, 1.5, 1.5, 5))
  plot(map, col = col.features, bg = col.bg, border = bord, 
       main = unique(biogeo$species), font.main = 3)
  
  if (plot.range) {
    nat_rang <- range[which(range$legend == "Extant (resident)"), ]
    alien_rang <- range[which(range$legend == "Introduced"), ]
    
    plot(nat_rang, add = TRUE, border = NA, col = "#31a354")
    plot(alien_rang, add = TRUE, border = NA, col = "#feb24c")
  }
  
  if (box) {
    plot(CP, add = TRUE)
    points((extent(CP)[1] + extent(CP)[2])/2, extent(CP)[3], pch = 3, 
           cex = 0.8)
    text((extent(CP)[1] + extent(CP)[2])/2,
         extent(CP)[3] - (extent(CP)[4] - extent(CP)[3])/22, 
         round((extent(CP)[1] + extent(CP)[2])/2, 2), cex = 0.8)
    points(extent(CP)[2], (extent(CP)[3] + extent(CP)[4])/2, pch = "_")
    text(extent(CP)[2] + (extent(CP)[2] - extent(CP)[1])/40, 
         (extent(CP)[3] + extent(CP)[4])/2, 
         round((extent(CP)[3] + extent(CP)[4])/2, 2), cex = 0.8, srt = 90)
    points(extent(CP)[2] - (extent(CP)[2] - extent(CP)[1])/20, 
           extent(CP)[3] + (extent(CP)[4] - extent(CP)[3])/20, pch = 24, 
           bg = "gray30", cex = 1.5)
    text(extent(CP)[2] - (extent(CP)[2] - extent(CP)[1])/20,
         extent(CP)[3] + (extent(CP)[4] - extent(CP)[3])/9, "N", cex = 0.6)
    gradientLegend(valRange = c(0, 1), pos = c(1.03, 0.6, 1.045, 0.89), 
                   side = 4, 
                   color = colbiogeo(20),
                   n.seg = 3)
  } else {
    gradientLegend(valRange = c(0, 1), pos = c(1.03, 0.2, 1.045, 0.8),
                   side = 4,
                   color = colbiogeo(20), 
                   n.seg = 3)
  }
  
  points(biogeo_sp, pch = 21, cex = 1, bg = col_pts_biogeo)
}
