library(goodpractice)
library(devtools)

gp("/Users/carloseduardoaribeiro/Documents/bRacatus/bRacatus") #change 


#R CMD check
devtools::check_built(path = "/Users/carloseduardoaribeiro/Documents/bRacatus/bRacatus", 
                      manual = T, cran = T)

devtools::spell_check(use_wordlist = T)

# Run R CMD --check, make sure that there are no errors, warnings or notes

devtools::check_rhub()

devtools::check_win_devel()

codemeta::write_codemeta()

devtools::release()

# create a release on github: https://github.com/EduardoArle/bRacatus/tags

# Submit to Ropensci: https://github.com/ropensci/software-review 

library(roxygen2)
library(devtools)
document()
