[![Build Status](https://travis-ci.org/EduardoArle/bRacatus.svg?branch=master)](https://travis-ci.org/EduardoArle/bRacatus)
![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)
![CRAN](https://www.r-pkg.org/badges/version/bRacatus)
![CRAN - Downloads](http://cranlogs.r-pkg.org/badges/grand-total/bRacatus?[color=pink])
![CRAN - Downloads Month](http://cranlogs.r-pkg.org/badges/Rcpp)
![GitHub: Closed Issues](https://img.shields.io/github/issues-closed-raw/EduardoArle/bRacatus)

# bRacatus v1.0.4

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
