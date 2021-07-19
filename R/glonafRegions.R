#' glonafRegions
#'
#' Gets regions listed by GloNAF for plant species
#'
#' @importFrom rgdal readOGR
#' @importFrom raster area
#' @importFrom maptools spRbind
#' @importFrom sp spChFIDs
#' @param species character, species binomial name
#' @param native character, source for the native reference regions. Options 
#' are "gift", "range map", or "checklist". If "gift" is chosen, the function 
#' will automatically download native regions listed by GIFT for the species. If
#' "range map" or "checklist" is chosen, the user must provide a shapefile with 
#' either the species range map, or the features representing regions where it 
#' has been listed as native. Default is "gift" 
#' @param wd_glonaf character, path to a folder containing the glonaf shapefile 
#' and occurrence table.
#' @return This function returns a list containing three shapefiles derived by 
#' information supplied by GloNAF for the alien reference regions, and the
#' chosen source for the native reference regions. "regs" includes all the 
#' features corresponding to regions where the species has been listed as 
#' present. "regs_native" includes all the features corresponding to regions 
#' where the species has 
#' been listed as native. And "regs_alien" includes all the features 
#' corresponding to regions where the species has been listed as alien.
#' @examples
#' gift_reference_regions <- giftRegions("Boreava aptera")
#' @export
glonafRegions <- function(species,native = "gift",wd_glonaf){
  
  if(native == "gift"){
    regs_nat <- giftRegions(species)
    regs_nat2 <- try(regs_nat$Native,silent=T)
  }else{
    #create options for checklists or range maps input by the user
  }
  
  #change the attribut table to show only area
  if(class(regs_nat2) == "try-error" | nrow(regs_nat2) == 0){
    nat <- NA
  }else{
    regs_nat2@data <- data.frame(area = area(regs_nat2)/1000000)
    
    nat <- sp::spChFIDs(regs_nat2,paste(c(1:nrow(regs_nat2))))
  }
  
  #load glonaf information
  setwd(wd_glonaf)
  
  tab_glo <- read.csv("GloNAF_modified_table.csv")
  sps_tab <- tab_glo[which(tab_glo$standardized_name == species),]
  
  glonaf_shp <- readOGR("GloNAF_modified", dsn = wd_glonaf)
  
  regs_alien <- glonaf_shp[which(glonaf_shp$OBJIDsic %in%
                                sps_tab$OBJIDsic),]
  
  if(nrow(regs_alien) > 0){
    #change the attribut table to show only area
    regs_alien@data <- data.frame(area = area(regs_alien)/1000000)
    
    alien <- sp::spChFIDs(regs_alien,paste(c(nrow(regs_nat2)+1 : nrow(regs_alien))))
    
    regs <- spRbind(nat,alien)
    regs_native <- nat
    regs_alien <- alien
    regs_list <- list(regs,regs_native,regs_alien)
    names(regs_list) <- c("Presence","Native","Alien")
    return(regs_list)
  }else{
    return("Species not listed in GloNAF")
  }
}
