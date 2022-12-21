#' signalCalculation
#'
#' Calculates signals sent from reference regions to point records.
#'
#' @importFrom raster extract
#' @param ref_reg a list of shapefiles containing checklist regions as 
#' "presence", "native", and "alien" categories. These can be original 
#' checklists, or checklists derived from range maps.
#' @param pts data.frame containing the point records and their coordinates.
#' @param biogeo logical, whether the biogeographical status indices should 
#' be calculated or not. Default is true, however at least the native 
#' reference regions must be included in the data. 
#' @return The data.frame of species occurrences with extra columns containing
#' the location ID and presence signals for each point. If biogeo=TRUE, the 
#' data.frame also includes the nativeness and alienness indices.
#'
#' @export
signalCalculation <- function(ref_reg, pts, biogeo = TRUE) {
  
  ref_reg_rast <- rasteriseChecklists (ref_reg)
  ref_reg_ID <- valueID (ref_reg_rast)
  occ_ID <- occID (pts)
  
  sps_range_ID <- ref_reg_ID$Presence$cell_ID  
  #IDs of the cells overlapping reference regions
  sps_range_prior_conf <- ref_reg_ID$Presence$prob  
  #a priori confidence of occurrence in each cell
  pr_index <- rep(-9999,nrow(pts))
  
  if (biogeo == TRUE) {
    nat_index <- rep(-9999,nrow(pts))
    alien_index <- rep(-9999,nrow(pts))
  }
  
  #occ_ID$ID_points[i]
  unique_IDs <- unique(occ_ID$ID_points)
  for (i in seq_along(unique_IDs)) 
    {
    con <- gzcon(url(paste0("http://gift.uni-goettingen.de/bracatus/distances/",
                  unique_IDs[i])))
    dist <- try(suppressWarnings(readRDS(con)),silent =TRUE)
    
    if(inherits(dist, "try-error")){
      
      message("Distance matrices not accessible due to connection issues.")
      return(NULL)
      
    }else{
      close(con)

      prox <- 1 - (dist[sps_range_ID]/200)
      #normalise the distances and invert the values to calculate a proximity 
      #index between 0 and 1, getting only the values for the cells that 
      #send a signal
      
      if (unique_IDs[i] %in% sps_range_ID) {
        # this part checks whether the point is in a cell that sends a signal, 
        #to include the majoration of the signal sent from the same cell
        pts_value <- sps_range_prior_conf[which(sps_range_ID == 
                                                  unique_IDs[i])]
        #gets the index in the point location
      } else {
        pts_value <- 0
      }
      
      pr_index[which(occ_ID$ID_points == unique_IDs[i])] <- 
        sum(sps_range_prior_conf * (prox^32), 
                         na.rm = TRUE) + pts_value * 9
      
      if (biogeo == TRUE) {
        sps_nat_range_ID <- ref_reg_ID$Native$cell_ID  
        #IDs of the cells overlapping refference regions
        sps_nat_range_prior_conf <- ref_reg_ID$Native$prob  
        #a priori confidence of occurrence in each cell
        
        nat_prox <- 1 - (dist[sps_nat_range_ID]/200)  
        #normalise the distances and invert the values to calculate a proximity 
        #index between 0 and 1, getting only the values for the cells 
        #that send a signal
        
        if (unique_IDs[i] %in% sps_nat_range_ID) {
          # this part checks whether the point is in a cell that sends a signal,
          #to include the majoration of the signal sent from the same cell
          pts_value <- sps_nat_range_prior_conf[which(sps_nat_range_ID == 
                                                        unique_IDs[i])]  
          #gets the index in the point location
        } else {
          pts_value <- 0
        }
        
        nat_index[which(occ_ID$ID_points == unique_IDs[i])] <- 
          sum(sps_nat_range_prior_conf * (nat_prox^32), 
                            na.rm = TRUE) + pts_value * 9
        
        sps_alien_range_ID <- ref_reg_ID$Alien$cell_ID  
        #IDs of the cells overlapping refference regions
        sps_alien_range_prior_conf <- ref_reg_ID$Alien$prob  
        #a priori confidence of occurrence in each cell
        
        alien_prox <- 1 - (dist[sps_alien_range_ID]/200)
        #normalise the distances and invert the values to calculate a proximity 
        #index between 0 and 1, getting only the values for the cells that 
        #send a signal
        
        if (unique_IDs[i] %in% sps_alien_range_ID) {
          # this part checks whether the point is in a cell that sends a signal,
          #to include the majoration of the signal sent from the same cell
          pts_value <- sps_alien_range_prior_conf[which(sps_alien_range_ID ==
                                                          unique_IDs[i])]
          #gets the index in the point location
        } else {
          pts_value <- 0
        }
        
        alien_index[which(occ_ID$ID_points == unique_IDs[i])] <- 
          sum(sps_alien_range_prior_conf * (alien_prox^32), 
                              na.rm = TRUE) + pts_value * 9
      }
    }
  }
  
  signal32_10 <- pr_index/max(pr_index)
  
  df <- data.frame(signal32_10 = signal32_10)
  
  if (biogeo == TRUE) {
    signal_native32_10 <- nat_index/max(nat_index)
    signal_alien32_10 <- alien_index/max(alien_index)
    df <- data.frame(signal32_10 = signal32_10, 
                     signal_native32_10 = signal_native32_10, 
                     signal_alien32_10 = signal_alien32_10)
  }
  
  output <- cbind(occ_ID, df)
  return(output)
}
