bRacatus 1.1.1 (2024-04-25)
=========================
  
### MAJOR IMPROVEMENTS
  * Adapted all functions plotting maps to deal with the sf problem of 'edge crosses edge'
  

bRacatus 1.1.0 (2023-10-25)
=========================
  
### MAJOR IMPROVEMENTS
  * Adapted all functions using the spatial packages rgdal, rgeos, and maptools to work without these dependencies. 
  

bRacatus 1.0.11 (2022-12-21)
=========================
  
### MINOR IMPROVEMENTS
  * Functions getOcc and signalCalculation now fail gracefully if the internet connection to the database is unavailable. 


bRacatus 1.0.10 (2022-12-20)
=========================

### MINOR IMPROVEMENTS
  * Changed authors e-mail, and cleaned code to avoid lines with more than 80 characters. 
  

bRacatus 1.0.9 (2022-09-16)
=========================
  
### MINOR IMPROVEMENTS
  * Fixed bug in function glonafRegions, that were producing errors due to a change in the GIFT database. 
  

bRacatus 1.0.8 (2022-08-09)
=========================
  
### MINOR IMPROVEMENTS
  * Fixed bug in functions rangeMap and biogeoStatus, that were producing errors when no alien range was provided. 
  

bRacatus 1.0.7 (2021-12-07)
=========================
  
### MINOR IMPROVEMENTS
  * Fixed the problem in that was preventing the vignette from being built and causing some tests to fail. 


bRacatus 1.0.6 (2021-11-09)
=========================
  
### MINOR IMPROVEMENTS
  * Changed the error message in the functions that use internet resources to an informative message, according to requirements made by CRAN. 


bRacatus 1.0.5 (2021-10-21)
=========================

### MAJOR IMPROVEMENTS
  * function glonafRegions included in the package to automatically retrieve data from GloNAF database.
  
### MINOR IMPROVEMENTS
  * fix in the function countryChecklist to allow plotting of the results by function plotRefReg.


bRacatus 1.0.4 (2021-05-28)
=========================

### MAJOR IMPROVEMENTS
  * Changed the "closeAllConnections()" in signalCalculation function to closing only the connections that were open, according to instructions received from CRAN staff. 
  * Fixed bug reported by user in accuracy function.


bRacatus 1.0.3 (2021-04-17)
=========================

### MAJOR IMPROVEMENTS
  * Function giftRegion, which caused the tests to fail, is now working.


bRacatus 1.0.2 (2021-01-15)
=========================

### MAJOR IMPROVEMENTS
  * Fixed bugs reported by users
  
### MINOR IMPROVEMENTS
  * Edited citations


bRacatus 1.0.1 (2020-12-10)
=========================

### MAJOR IMPROVEMENTS
  * Reformulated code according to comments from CRAN staff
  * Made the changes suggested by reviewers


bRacatus 1.0.0 (2020-08-18)
=========================

### MAJOR IMPROVEMENTS
  * Added a README.md
  * Added vignette
  * Added NEWS.md
  * Added Travis integration
  * Adapted code to good practice
  
### MINOR IMPROVEMENTS
  * Added citation
  * Added CRAN comments
  * Added travis badge to README
  * reformatted code
  
