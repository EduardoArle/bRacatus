#' plotBiogeoStatus
#'
#' Plot the species occurrences showing the estimated biogeographical status 
#' of points.
#'
#' @importFrom graphics points par text
#' @importFrom plotfunctions gradientLegend
#' @importFrom raster extent plot
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_intersects st_polygon st_sfc st_transform
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
  world <- ne_countries(returnclass = "sf")
  old_proj <- crs(world)
  world <- st_transform(world, crs = 3857)
  
  biogeo_sf <- st_as_sf(biogeo, 
                        coords = c('decimalLongitude', 'decimalLatitude'),
                        crs = old_proj)
  biogeo_sf <- st_transform(biogeo_sf, crs = 3857)
  
  if(regional){
    if(reg.by == "country"){

      countries <- unique(vapply(st_intersects(biogeo_sf,world), 
                          function(x) if (length(x)==0) NA_integer_ else x[1],
                          FUN.VALUE = 1))
      
      
      if(length(which(is.na(countries))) == 1){
        countries <-  countries[-which(is.na(countries))]
      }
      
      countries <- world[countries,]
      
      coords_CP <- extent(countries)
      
      CP <- st_polygon(list(cbind(
        c(coords_CP[1],coords_CP[2],coords_CP[2],coords_CP[1],coords_CP[1]),
        c(coords_CP[3],coords_CP[3],coords_CP[4],coords_CP[4],coords_CP[3]))))
      
      CP <- st_sfc(CP, crs = crs(world))
      CP <- st_as_sf(CP)
      
      map <- suppressWarnings(st_intersection(world, CP))
    }
    
    if (reg.by == "points") {
      
      coords_CP <- extent(biogeo_sf)
      
      x_marge <- (coords_CP[2] - coords_CP[1]) * 0.05
      y_marge <- (coords_CP[4] - coords_CP[3]) * 0.05
      
      CP <- st_polygon(list(cbind(
        c(coords_CP[1],coords_CP[2],coords_CP[2],coords_CP[1],coords_CP[1]) +
          c(-x_marge, x_marge, x_marge, -x_marge, -x_marge),
        c(coords_CP[3],coords_CP[3],coords_CP[4],coords_CP[4],coords_CP[3]) +
          c(-y_marge, -y_marge, y_marge, y_marge, -y_marge))))
      
      CP <- st_sfc(CP, crs = crs(world))
      CP <- st_as_sf(CP)
      
      map <- suppressWarnings(st_intersection(world, CP))
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
  
  map <- st_transform(map, crs = old_proj)
  
  par(mfrow = c(1, 1), mar = c(1.5, 1.5, 1.5, 5))
  plot(st_geometry(map), col = col.features, bg = col.bg, border = bord, 
       main = unique(biogeo$species), font.main = 3)
  
  if (plot.range) {
    nat_rang <- range[which(range$legend == "Extant (resident)"), ]
    alien_rang <- range[which(range$legend == "Introduced"), ]
    
    nat_rang <- st_as_sf(nat_rang, crs = old_proj)
    alien_rang <- st_as_sf(alien_rang, crs = old_proj)
    
    plot(st_geometry(nat_rang), add = TRUE, border = NA, col = "#31a354")
    plot(st_geometry(alien_rang), add = TRUE, border = NA, col = "#feb24c")
  }
  
  if (box) {
    
    CP <- st_transform(CP, crs = old_proj)
    
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
  
  biogeo_sf <- st_transform(biogeo_sf, crs = old_proj)
  
  plot(st_geometry(biogeo_sf), add = T, pch = 21, cex = 1, bg = col_pts_biogeo)
}
