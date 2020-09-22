#' plotRefRef
#'
#' Plot the species reference regions with map background for visualisation
#'
#' @importFrom raster plot
#' @importFrom rworldmap getMap
#' @importFrom sp proj4string
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
  world <- getMap(resolution = "low")
  
  par(mfrow=c(2,2),mar=c(0.6,0.6,0.6,0.6))

  plot(world, col = "gray90", main = "Presence", border = NA)
  transp <- ((1 - (ref_reg$Presence$area/1.7e+07)) * 0.99)^10
  plot(ref_reg$Presence, col = rgb(0, 0, 1, transp), border = NA, add = TRUE)
  
  plot(world, col = "gray90", main = "Native", border = NA)
  transp <- ((1 - (ref_reg$Native$area/1.7e+07)) * 0.99)^10
  plot(ref_reg$Native, col = rgb(0, 0.4, 0, transp), border = NA, add = TRUE)
  
  plot(world, col = "gray90", main = "Alien", border = NA)
  transp <- ((1 - (ref_reg$Alien$area/1.7e+07)) * 0.99)^10
  plot(ref_reg$Alien, col = rgb(1, 0.5, 0, transp), border = NA, add = TRUE)
}

