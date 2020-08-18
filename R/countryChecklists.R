#' countryChecklists
#'
#' Prepares user provided reference regions on a country level
#'
#' @importFrom rworldmap getMap
#' @param  countries vector with one or more country names
#' @param  biogeo_status vector informing the status of each country: alien, native or unknown
#' @return This function provides shapefiles of countries with the correspondent biogeographic status of the species.
#' @examples
#' country_checklist <- countryChecklist(c("Brazil","Argentina","Uruguay","Paraguay"),
#'                                       c("native","alien","unknown","native"))
#' @export

R version 4.0.2 (2020-06-22) -- "Taking Off Again"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.


Restarting R session...

> library(bRacatus)
> citation("bRacatus")
Error in .bibentry_check_bibentry1(rval) : 
  A bibentry of bibtype ‘Article’ has to specify the fields: journal, year

Restarting R session...

> library(bRacatus)
> library(bRacatus)
> citation("bRacatus")

Arle E, Zizka A, Keil P, Winter M, Essl F, Knight T, Weigelt P, Jiménez-Muñoz M, Meyer C (2020). “bRacatus: estimating the accuracy and biogeographical status of georeferenced
biological data.” _submitted_.

A BibTeX entry for LaTeX users is

@Article{,
  title = {bRacatus: estimating the accuracy and biogeographical status of georeferenced biological data},
  author = {Eduardo Arle and Alexander Zizka and Petr Keil and Marten Winter and Franz Essl and Tiffany Knight and Patrick Weigelt and Marina Jiménez-Muñoz and Carsten Meyer},
  journal = {submitted},
  year = {2020},
}

> library(formatR)
> clean_source()
Error in clean_source() : could not find function "clean_source"
> tidy_source()
accuracy <- function(signals) {
  model <- bRacatus::Model_accuracy
  acc <- predict(model, signals, type = "response")
  tab <- signals[, -((ncol(signals) - 3):ncol(signals))]
  tab <- cbind(tab, accuracy = acc)
  return(tab)
}
> ?tidy_source

Restarting R session...

> tidy_source(arrow = TRUE)
availableCountries <- function() {
  world <- getMap()
  list <- sort(world$NAME)
  return(list)
}

> tidy_source(arrow = TRUE)
availableCountries <- function() {
  world <- getMap()
  list <- sort(world$NAME)
  return(list)
}
> tidy_source(arrow = TRUE)
biogeoStatus <- function(signals) {
  model <- bRacatus::Model_biogeo
  biogeo <- predict(model, signals, type = "response")
  tab <- signals[, -((ncol(signals) - 3):ncol(signals))]
  tab <- cbind(tab, biogeoStatus = biogeo)
  return(tab)
}
> tidy_source(arrow = TRUE)
countryChecklist <- function(countries, biogeo_status) {
  if (length(countries) != length(biogeo_status)) {
    stop("countries and biogeo_status have different lengths")
  }
  world <- getMap()
  features <- numeric()
  for (i in 1:length(countries)) {
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
  native <- world[features[which(biogeo_status == "native")], ]
  alien <- world[features[which(biogeo_status == "alien")], ]
  
  range_list <- list(presence, native, alien)
  names(range_list) <- c("Presence", "Native", "Alien")
  return(range_list)
}