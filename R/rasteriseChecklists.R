#' rasteriseChecklists
#'
#' Transforms checklist shapefiles into rasters
#'
#' @import rgdal
#' @import raster
#' @param  checklists list containing checklists for the presence, alien and native reference regions
#' @return Converts lists of checklsits, categorised as "Presence", "Native" or "Alien" into half a degree rasters informing the prior confidence of detection in each cell.
#' @examples
#' country_checklist <- countryChecklist(c("Brazil","Argentina","Uruguay","Paraguay"),
#' c("native","alien","unknown","native"))
#'
#' rasterised_checklist <- rasteriseChecklists(country_checklist)
#' @export
rasteriseChecklists <- function(checklists){
  raster_half_degree <- raster(vals=NA,res=.5)   #create an empty raster with half degree grid
  raster_half_degree <- projectRaster(raster_half_degree,crs=projection(checklists[[1]]))  #insert same projection as in the checklists
  checklists_raster <- list()
  for(i in 1:length(checklists)) #calculate the prior confiability of occurrence per cell
  {
    if(nrow(checklists[[i]])!=0){
      layers <- list()
      for(j in 1:nrow(checklists[[i]]))
      {
        raster_cut <- crop(raster_half_degree,extent(checklists[[i]][j,])+c(-2,2,-2,2))
        raster1 <- rasterize(checklists[[i]][j,],raster_cut,getCover=T) #rasterise the checklists #count also very small features
        raster1[] <- ifelse(raster1[]>0,1,NA)
        raster2 <- extend(raster1,raster_half_degree)
        raster2[] <- ifelse(is.na(raster2[]),0,1/length(which(!is.na(raster2[]))))
        layers[[j]] <- raster2
      }
    }else{
      layers <- raster(vals=0,res=.5)
    }
    inv_raster <- lapply(layers,function(x){1-x}) #invert the values in the raster to calculate the probability of not occurring
    checklists_raster[[i]] <- 1-prod(stack(inv_raster))  #calculate the combined prior confiability of occurrence according to the formula: p = 1-(pn1*pn2 … pnn)
  }
  names(checklists_raster) <- names(checklists)
  return(checklists_raster)
}
