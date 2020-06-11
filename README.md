# lactModel
To model the lactation curve of (mountain-pastured) dairy cows

This repository is a pseudo R-package, that is built like a package but has never been submitted to the CRAN or other package repository.

To use it you can either
  1) install as a standard R package it using the following R-command (require devtools package):
  devtools::install_github('SolangeD/lactModel')
  
  2) copy the file in the R folder and run it with R to save the functions locally 
  (you will need to rerun the script everytime you start a new R-session since it is not saved as a package)
  
The way to use functions created in this package is defined in the lactModel-manual.pdf  

In the /inst/extdata you also have the rest of the code used in the publication (filtering of the data, link between environment and milk records etc...)

This code is free ; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation ; either version 3 of the License, or (at your option) any later version.
It is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY ; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details http://www.gnu.org/licenses/gpl-3.0.html.
