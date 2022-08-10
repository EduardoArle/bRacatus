#' @importFrom data.table rbindlist
#' @importFrom raster extract raster projectRaster projection crop
#' @importFrom raster rasterize extend stack extent
#'
getGbifDecade <- function(species){
  pts_1001_1900 <- occ_search(scientificName=species,  
                              #download gbif records per period
                              limit=200000,year='1001,1900')
  if(!inherits(pts_1001_1900[[3]], "NULL")){
    if(nrow(pts_1001_1900[[3]])==200000){     
      #flag possible periods with more than 200000 records
warning("There are more than 200,000 records available between 1001 and 1009.
         Not all have been downloaded.")
    }
  }
  pts_decade <- list()
  for(k in 1:ceiling(((as.numeric(format(Sys.time(),"%Y")))-1900)/10))
  {
    pts_decade[[k]] <- occ_search(scientificName=species,  
                                  #download gbif records per period
                  limit=200000,year=paste(1891+10*k,",",1890+10*k+10,sep=""),
                  hasCoordinate=TRUE)
    if(!is.null(nrow(pts_decade[[k]][[3]]))){
      if(nrow(pts_decade[[k]][[3]])==200000){
        pts_decade[[k]][[3]] <- getGbifYear(species,1891+10*k,1890+10*k+10)
      }
    }
  }
  gbif_rec[[3]] <- rbind(pts_1001_1900[[3]],rbindlist(lapply(pts_decade,
                                  function(x){x[[3]]}),fill=TRUE),fill=TRUE)
}

getGbifYear <- function(species,year_0,year_1){
  #this function loops though year_0 to year_1 downloading all GBIF 
  #records for a species
  pts_year <- list()
  for(l in 1:(year_1-year_0+1))
  {
    pts_year[[l]] <- rgbif::occ_search(scientificName=species,  
                                       #download gbif records per year
                                       limit=200000,year=paste(year_0-1+l),
                                       hasCoordinate=TRUE)
    if(!is.null(nrow(pts_year[[l]][[3]]))){
      if(nrow(pts_year[[l]][[3]])==200000){
        pts_year[[l]][[3]] <- getGbifMonth(species,year_0-1+l)
      }
    }
  }
  pts2 <- rbindlist(lapply(pts_year,function(x){x[[3]]}),fill=TRUE)
  return(pts2)
}

getGbifMonth <- function(species,year){
  #this function loops though each month downloading GBIF 
  #records for a species for a given year
  pts_month <- list()
  for(m in 1:12)
  {
    pts_month[[m]] <- rgbif::occ_search(scientificName=species,  
                                        #download gbif records per year
                                        limit=200000,year=year,month=m,
                                        hasCoordinate=TRUE)
    if(!is.null(nrow(pts_month[[m]][[3]]))){
      if(nrow(pts_month[[m]][[3]])==200000){
        warning(paste0(
          "There are more than 200,000 occurrences for the month ",m,
          "/",year,". Not all have been downloaded"))
      }
    }
  }
  pts_month2 <- rbindlist(lapply(pts_month,function(x){x[[3]]}),fill=TRUE)
  return(pts_month2)
}

occID <- function(occ) {
  occ_sp <- occSpatialPoints(occ)
  ID <- raster(res = 1/2)
  ID[] <- c(seq_len(length(ID)))
  ID_points <- extract(ID, occ_sp)
  occ$ID_points <- ID_points
  return(occ)
}

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
      layers <- list(raster(vals=0,res=.5))
    }
    
    if(length(layers)>1){
      
      inv_raster <- lapply(layers,function(x){1-x}) 
      #invert the values in the raster to calculate the prob of not occurring
      
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

valueID <- function(checklists_raster) {
  ID_prob <- list()
  for (i in seq_along(checklists_raster)) {
    cell_ID <- which(checklists_raster[[i]][] != 0)
    prob <- checklists_raster[[i]][which(checklists_raster[[i]][] != 0)]
    ID_prob[[i]] <- data.frame(cell_ID = cell_ID, prob = prob)
  }
  names(ID_prob) <- names(checklists_raster)
  return(ID_prob)
}
