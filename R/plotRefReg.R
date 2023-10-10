#' plotRefRef
#'
#' Plot the species reference regions with map background for visualisation
#'
#' @importFrom raster plot
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_area st_geometry
#' @param ref_reg list containing three shapefiles derived by information 
#' supplied by GIFT. "regs" includes all the features corresponding to regions 
#' where the species has been listed as present. "regs_native" includes all 
#' the features corresponding to regions where the species has been listed as 
#' native. And "regs_alien" includes all the features corresponding to regions 
#' where the species has been listed as alien..
#' @return This function plots three maps of the species occurrence, showing 
#' the regions where it is present, native and alien.
#' @export
plotRefReg <- function(ref_reg) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  world <- ne_countries(returnclass = "sf")
  
  par(mfrow=c(2,2),mar=c(0.6,0.6,0.6,0.6))

  plot(st_geometry(world), col = "gray90", main = "Presence", border = NA)
  ref_reg$Presence$area <- as.numeric(st_area(ref_reg$Presence)/1000000)
  transp <- ((1 - (ref_reg$Presence$area/5e+07)) * 0.99)^10
  plot(st_geometry(ref_reg$Presence), col = rgb(0, 0, 1, transp), border = NA, 
       add = TRUE)
  
  plot(st_geometry(world), col = "gray90", main = "Native", border = NA)
  ref_reg$Native$area <- as.numeric(st_area(ref_reg$Native)/1000000)
  transp <- ((1 - (ref_reg$Native$area/5e+07)) * 0.99)^10
  plot(st_geometry(ref_reg$Native), col = rgb(0, 0.4, 0, transp), border = NA, 
          add = TRUE)
  
  plot(st_geometry(world), col = "gray90", main = "Alien", border = NA)
  ref_reg$Alien$area <- as.numeric(st_area(ref_reg$Alien)/1000000)
  transp <- ((1 - (ref_reg$Alien$area/5e+07)) * 0.99)^10
  plot(st_geometry(ref_reg$Alien), col = rgb(1, 0.5, 0, transp), border = NA, 
       add = TRUE)
}

