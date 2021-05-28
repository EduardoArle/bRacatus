[![Build Status](https://travis-ci.org/EduardoArle/bRacatus.svg?branch=master)](https://travis-ci.org/EduardoArle/bRacatus)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![All Downloads](https://img.shields.io/packagecontrol/dm/EduardoArle/bRacatus)
![CRAN](https://www.r-pkg.org/badges/version/bRacatus)

# bRacatus v1.0.2

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
