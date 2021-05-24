# FBMcw

This package contains R code and an interactive Shiny application associated with the generic form of fibre bundle models for soil reinforcement by plant roots (FBM, FBMcw) as developed by Meijer (2021).

v0.1.0 - May 2021 - Gerrit Meijer (<gjm36@bath.ac.uk>)


## Running the app online

An online app, showcasing the capabilities of the generic fibre bundle model formulation, is currently hosted through Shinyapps.io: <https://gjmeijer.shinyapps.io/FBMcw/>


## Package and installation

This R package contains:

- an interactive app (using R Shiny), interactively showing how root reinforcement predictions change when varying input parameters.
- all functions required to make root reinforcement calculations according to the various fibre bundle models

To install this package on your local machine:

1. Open R. (If not installed, I recommend installing RStudio (free software) that can be downloaded from https://www.rstudio.com/products/rstudio/download/)
2. If not already installed, install the `devtools` package by typing `install.packages("devtools")` in the R console. This package allows you to interact with R packages hosted on GitHub (among many other things).
3. Install the `FBMcw` package by typing `devtools::install_github("GJMeijer/FBMcw")` in the R console


## Running the app offline

To run the included app, type `FBMcw::run_app()` in the R console. This will open an interactive App in your default browser. This app relies on R continuing to run in the background for any computations.

This app shows an interactive version of the various fibre bundle models, capable of showing how root reinforcement predictions change when altering input parameters in real time. This app is constructed using the R package `shiny`.


## Using calculations functions

The `RBMcw` package contains a large number of R functions that can be called to calculate peak root reinforcements or current reinforcements at a specific strain level. These can be called in R after loading the package into your current R session by using the command `require("FBMcw")`.

For each of the following functions, a help page can be opened by typing `?function_name` in the R console, where `function_name` is the name of the function you want to see the documentation of.


### Peak reinforcements (continuous diameters)

Functions to calculate peak root reinforcements, assuming continuous power-law distributions of the root area ratio and root tensile strength:

- `calc_cru_wwmc()`: calculate the peak reinforcement according to the Wu/Waldron model with continuous root distributions
- `calc_kku_fbmc()`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMc model. Multiply the result by the results from `calc_cru_wwmc` to calculate the peak reinforcement according to the FBMc.
- `calc_kku_fbmcw()`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMcw model. Multiply the result by the results from `calc_cru_wwmc` to calculate the peak reinforcement according to the FBMcw.


### Peak reinforcements (discrete diameter classes)

The package contains a number of functions to calculate peak root reinforcements, assuming root are binned in discrete classes:

- `calc_cru_wwm()`: calculate the peak reinforcement according to the Wu/Waldron model
- `calc_cru_fbm()`: calculate the peak root reinforcement according to the FBM model (the 'traditional' fibre bundle model)
- `calc_cru_fbmw()`: calculate the peak root reinforcement according to the FBMw model (the 'traditional' fibre bundle model with added Weibull survival functions)

To discretise a continuous distribution of roots into a number of discrete classes, you can use (or use your own functions):

- `discretise_rootclasses()`: takes a range of roots and splits it into a specified number of equal-width root diameter classes, assuming power-law distributions for root area ratio and root tensile strength.


### Mobilisation of root reinforcement

The following functions are available to calculat the current root reinforcement as function of strain:

- `calc_kk_fbmc()`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMc model at each value of strain
- `calc_kk_fbmcw()`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMcw model at each value of strain
- `calc_cr_fbm()`: calculate the current root reinforcement according to the FBM model at each value of strain (discrete root classes)
- `calc_cr_fbmw()`: calculate the current root reinforcement according to the FBMw model at each value of strain (discrete root classes)

To generate suitable ranges of strains, you can use:
(
- `generate_strainrange()`: generates a suitable range of strains to analyse the mobilisation of root reinforcement
- `generate_strainrange_fbm()`: generates a suitable range of strains to analyse the mobilisation of root reinforcement for traditional fibre bundle models. It returns strains just before and after each root failure only, to minimise the amount of computations required. 


## Copyright and Licence

Copyright (C) 2021 University of Bath

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.


## References

Meijer, G. J. 2021. "A Generic Form of Fibre Bundle Models for Root Reinforcement of Soil." Plant & Soil x(x):xx-xx. DOI:xxx
