#' plotRefRef
#'
#' Plot the species reference regions with map background for visualisation
#'
#' @importFrom raster plot
#' @importFrom rworldmap getMap
#' @param ref_reg list containing three shapefiles derived by information supplied by GIFT. "regs" includes all the features corresponding to regions where the species has been listed as present. "regs_native" includes all the features corresponding to regions where the species has been listed as native. And "regs_alien" includes all the features corresponding to regions where the species has been listed as alien..
#' @return This function plots three maps of the species occurrence, showing the regions where it is present, native and alien.
#' @export
plotRefReg <- function(ref_reg){
  world <- getMap()
  
  par(mfrow=c(2,2),mar=c(0.6,0.6,0.6,0.6))
  
  plot(world,col="khaki",bg="azure2",main="Presence",border=NA)
  plot(ref_reg$Presence,col="blue",add=T)
  
  plot(world,col="khaki",bg="azure2",main="Native",border=NA)
  plot(ref_reg$Native,col="darkgreen",add=T)
  
  plot(world,col="khaki",bg="azure2",main="Alien",border=NA)
  plot(ref_reg$Alien,col="orange",add=T)
}
