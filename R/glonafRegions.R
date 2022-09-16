#' glonafRegions
#'
#' Gets regions listed by GloNAF for plant species
#'
#' @importFrom rgdal readOGR
#' @importFrom raster area
#' @importFrom geojsonio geojson_read
#' @importFrom maptools spRbind
#' @importFrom sp spChFIDs
#' @param species character, species binomial name
#' @param native character, source for the native reference regions. Options 
#' are "gift", "range map", or "checklist". If "gift" is chosen, the function 
#' will automatically download native regions listed by GIFT for the species. If
#' "range map" or "checklist" is chosen, the user must provide a shapefile with 
#' either the species range map, or the features representing regions where it 
#' has been listed as native. Default is "gift".
#' @param nat_ref_reg shapefile containing either the species native range map
#' or checklist. The user must inform which reference region data type is being
#' provided in the parameter "native".
#' @return This function returns a list containing three shapefiles derived by 
#' information supplied by GloNAF for the alien reference regions, and the
#' chosen source for the native reference regions. "regs" includes all the 
#' features corresponding to regions where the species has been listed as 
#' present. "regs_native" includes all the features corresponding to regions 
#' where the species has been listed as native. And "regs_alien" includes all 
#' the features corresponding to regions where the species has been listed as 
#' alien.
#' @examples
#' glonaf_reference_regions <- glonafRegions("Ambrosia grayi")
#' @export
glonafRegions <- function(species,native = "gift",nat_ref_reg = NULL){
  
  if(native == "gift"){
    regs_nat <- giftRegions(species)
    regs_nat2 <- try(regs_nat$Native,silent=T)
  }
  
  if(native == "range map"){
    if(is.null(nat_ref_reg)){
      stop("Provide shapefile containing the species's native range.")
    }else{
      raster_2degrees <- raster(vals = NA, res = 2)
      raster_cut <- crop(raster_2degrees, extent(nat_ref_reg) + c(-2, 2, -2, 2))
      
      range_native2 <- rasterize(nat_ref_reg, raster_cut, getCover = TRUE)  
      #rasterise the range map #count also very small features
      range_native3 <- rasterToPolygons(range_native2)
      range_native4 <- range_native3[which(range_native3$layer != 0), ]
      range_native4$data <- 1
      range_native4 <- range_native4[which(range_native4$layer >= 0.05), ]
      range_native4$area <- raster::area(range_native4)/1e+06
      regs_nat2 <- range_native4
    }
  }
  
  #change the attribute table to show only area
  if(inherits(regs_nat2, "try-error") | nrow(regs_nat2) == 0){
    nat <- NA
  }else{
    regs_nat2@data <- data.frame(area = area(regs_nat2)/1000000)
    
    nat <- sp::spChFIDs(regs_nat2,paste(seq_len(nrow(regs_nat2))))
  }
  
  #get glonaf information
  
  genus <- gsub("(^.*) (.*$)","\\1",species)  #get genus
  epithet <- gsub("(^.*) (.*$)","\\2",species)  #get epithet
  
  con <- gzcon(url(paste0(
    "http://gift.uni-goettingen.de/bracatus/glonaf/species/",
    genus,"%20",epithet)))
 
  glonaf_table <- try(suppressWarnings(readRDS(con)),silent =TRUE)
 
  close(con)
  
  if(inherits(glonaf_table, "try-error")){
    
  message("GloNAF database currently not accessible due to server issues.
        Please try again later.")
    return(NULL)
    
  }else{
    if(nrow(glonaf_table)==0){warning(paste0(
      "GloNAF does not have checklists for ",
      species,"."))
    }else{
      for(i in seq_len(nrow(glonaf_table)))
      {
        #download the shapefile for each region
        a <- try(suppressWarnings(geojsonio::geojson_read(
          paste("http://gift.uni-goettingen.de/bracatus/glonaf/geojson/", 
                glonaf_table$OBJIDsic[i],".geojson",sep=""), what = "sp")),
          silent = TRUE)
        
        if(!inherits(a, "try-error")){
          b <- sp::spChFIDs(a,paste(i))
          if(i == 1){
            alien <- b
          }else{
            if(ncol(alien) == ncol(b)){
              alien <- spRbind(alien,b)
            }
          }
        }
      }
    }
  }
  
  if(nrow(alien) > 0){
    #change the attribute table of the alien regions to show only area
    alien@data <- data.frame(area = area(alien)/1000000)
    
    #fix the IDs to alow to later join with the native regions and make the 
    #Presence
    from <- nrow(nat)+1
    to <- nrow(nat)+nrow(alien)
    alien <- sp::spChFIDs(alien,paste(c(from:to)))
    presence <- spRbind(nat,alien)
    regs_list <- list(presence,nat,alien)
    names(regs_list) <- c("Presence","Native","Alien")
    return(regs_list)
  }else{
    return("Species not listed in GloNAF")
  }
}