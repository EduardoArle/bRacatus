#' rasteriseChecklists
#'
#' Transforms checklist shapefiles into rasters
#'
#' @importFrom raster raster
#' @importFrom raster projectRaster
#' @importFrom raster projection
#' @importFrom raster crop
#' @importFrom raster rasterize
#' @importFrom raster extend
#' @importFrom raster stack
#' @importFrom raster extent
#' @param  checklists list containing checklists for the presence, alien and 
#' native reference regions
#' @return Converts lists of checklists, categorised as "Presence", "Native" 
#' or "Alien" into half a degree rasters informing the prior confidence of 
#' detection in each cell.
#' @examples
#' country_checklist <- countryChecklist(
#' c("Brazil","Argentina","Uruguay","Paraguay"),
#' c("native","alien","unknown","native"))
#'
#' rasterised_checklist <- rasteriseChecklists(country_checklist)
#' @export
rasteriseChecklists <- function(checklists){
  raster_half_degree <- raster(vals=NA,res=.5)   
  #create an empty raster with half degree grid
  checklists_raster <- list()
  for(i in seq_along(checklists)) 
    #calculate the prior confiability of occurrence per cell
  {
    if(nrow(checklists[[i]])!=0){
      layers <- list()
      for(j in seq_len(nrow(checklists[[i]])))
      {
        raster_cut <- crop(raster_half_degree,
                           extent(checklists[[i]][j,])+c(-2,2,-2,2))
        raster1 <- rasterize(checklists[[i]][j,],raster_cut,getCover=TRUE) 
        #rasterise the checklists #count also very small features
        raster1[] <- ifelse(raster1[]>0,1,NA)
        raster2 <- extend(raster1,raster_half_degree)
        raster2[] <- ifelse(is.na(raster2[]),0,
                            1/length(which(!is.na(raster2[]))))
        layers[[j]] <- raster2
      }
    }else{
      layers <- raster(vals=0,res=.5)
    }
    
    inv_raster <- lapply(layers,function(x){1-x}) 
#invert the values in the raster to calculate the probability of not occurring
    
    if(length(inv_raster)>1){
      checklists_raster[[i]] <- 1-prod(stack(inv_raster))  
      #calculate the combined prior confiability of occurrence 
      #according to the formula: p = 1-(pn1*pn2 … pnn)
    }else{
      checklists_raster[[i]] <- layers[[1]]
    }
  }
  names(checklists_raster) <- names(checklists)
  return(checklists_raster)
}
