[![Build Status](https://travis-ci.org/EduardoArle/bRacatus.svg?branch=master)](https://travis-ci.org/EduardoArle/bRacatus)
![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)
![CRAN](https://www.r-pkg.org/badges/version/bRacatus)
![CRAN - Downloads](http://cranlogs.r-pkg.org/badges/grand-total/bRacatus?color=ff69b4)
![CRAN - Downloads Month](http://cranlogs.r-pkg.org/badges/bRacatus)
![CRAN - Downloads Week](http://cranlogs.r-pkg.org/badges/last-week/bRacatus?color=orange)
![GitHub: Closed Issues](https://img.shields.io/github/issues-closed-raw/EduardoArle/bRacatus)
[![DOI:10.1111/2041-210X.13629](https://zenodo.org/badge/DOI/10.1111/2041-210X.13629.svg)](https://doi.org/10.1111/2041-210X.13629)


# bRacatus v1.0.10

An R package to estimate the probability of species geographic point occurrence records being native or invasive and correct or erroneous.

## Installation

```{r}
# Install the stable CRAN version

install.packages("bRacatus")

# Install the development version

if(!require("remotes")) {
  
  install.packages("remotes")
  
}

remotes::install_github("EduardoArle/bRacatus")
```

## Usage

## Citation

```{r}
library(bRacatus)
citation("bRacatus")
```
