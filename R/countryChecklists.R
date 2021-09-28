#' countryChecklists
#'
#' Prepares user provided reference regions on a country level
#'
#' @importFrom raster area
#' @importFrom rworldmap getMap
#' @param  countries vector with one or more country names
#' @param  biogeo_status vector informing the status of each country: alien, 
#' native or unknown
#' @return This function provides shapefiles of countries with the 
#' correspondent biogeographic status of the species.
#' @examples
#' country_checklist <- countryChecklist(
#'                          c("Brazil","Argentina","Uruguay","Paraguay"),
#'                          c("native","alien","unknown","native"))
#' @export

countryChecklist <- function(countries, biogeo_status) {
  if (length(countries) != length(biogeo_status)) {
    stop("countries and biogeo_status have different lengths")
  }
  world <- getMap()
  features <- numeric()
  for (i in seq_along(countries)) {
    a <- grep(countries[i], world$NAME)
    if (length(a) == 0) {
      stop(paste0(countries[i], 
                  " was not found. Check  'availableCountries()'"))
    }
    if (length(a) > 1) {
      stop(paste0(countries[i],
    " corresponds to two or more countries. Check 'availableCountries()'"))
    }
    features[i] <- a
  }
  shp <- world[features, ]
  presence <- shp
  presence$area <- area(presence)/1000000
  native <- world[features[which(biogeo_status == "native")], ]
  native$area <- area(native)/1000000
  alien <- world[features[which(biogeo_status == "alien")], ]
  alien$area <- area(alien)/1000000
  
  range_list <- list(presence, native, alien)
  names(range_list) <- c("Presence", "Native", "Alien")
  return(range_list)
}
