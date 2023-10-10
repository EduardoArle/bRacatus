#' giveRegions
#'
#' Input checklist regions
#'
#' @importFrom rnaturalearth ne_countries
#' @param regs shapefile containing all regions of occurrence.
#' @param regs_native shapefile containing regions where the species is native.
#' @param regs_alien shapefile containing regions where the species is alien.
#' @return This function returns a list containing three shapefiles derived by 
#' information supplied by GIFT. "regs" includes all the features corresponding
#' to regions where the species has been listed as present. "regs_native" 
#' includes all the features corresponding to regions where the species has 
#' been listed as native. And "regs_alien" includes all the features 
#' corresponding to regions where the species has been listed as alien.
#' @examples
#' library(rnaturalearth)
#' world <- ne_countries(returnclass = "sf")
#' reg_names <- c("Brazil","Argentina","Uruguay","Paraguay")
#' reg_native <- c("Brazil","Paraguay")
#' reg_alien <- c("Argentina")
#' regs <- world[which(world$name_sort %in% reg_names),]
#' regs_native <- world[which(world$name_sort %in% reg_native),]
#' regs_alien <- world[which(world$name_sort %in% reg_alien),]
#' regs_list <- giveRegions(regs,regs_native,regs_alien)
#' @export
giveRegions <- function(regs,regs_native,regs_alien){

    regs_list <- list(regs,regs_native,regs_alien)
    names(regs_list) <- c("Presence","Native","Alien")
    return(regs_list)
}
