library(devtools)

install_github("MangoTheCat/goodpractice")
library(goodpractice)

gp("C:/Users/az64mycy/Dropbox (iDiv)/research_projects/00_bracatus/bRacatus")

#R CMD check
devtools::check_built(path = "", 
                      manual = T, cran = T)

devtools::spell_check()

devtools::check_rhub()

devtools::check_win_devel()

codemetar::write_codemeta()

devtools::release()


# create a release on github: https://github.com/EduardoArle/bRacatus/tags

# Submit to Ropensci: https://github.com/ropensci/software-review 
