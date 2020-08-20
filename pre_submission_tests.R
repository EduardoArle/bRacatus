library(goodpractice)

gp("C:/Users/ca13kute/Documents/bRacatus/Package/bRacatus") #change 

#R CMD check
devtools::check_built(path = "C:/Users/ca13kute/Documents/bRacatus/Package/bRacatus", 
                      manual = T, cran = T)

devtools::spell_check(use_wordlist = T)

# Run R CMD --check, make sure that there are no erros, warnings or notes

devtools::check_rhub()

devtools::check_win_devel()

codemetar::write_codemeta()

devtools::release()


# create a release on github: https://github.com/EduardoArle/bRacatus/tags

# Submit to Ropensci: https://github.com/ropensci/software-review 


###

update_wordlist(pkg = "C:/Users/ca13kute/Documents/bRacatus/Package/bRacatus", vignettes = TRUE, confirm = TRUE)
get_wordlist(pkg = "C:/Users/ca13kute/Documents/bRacatus/Package/bRacatus")

