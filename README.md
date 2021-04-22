# FBMcw

Generic fibre bundle models for soil reinforcement by plant roots

April 2021 - Gerrit Meijer (<gjm36@bath.ac.uk>)

This R package contains:

- an interactive app (using R Shiny), interactively showing how root reinforcement predictions change when varying input parameters.
- all functions required to make root reinforcement calculations according to the various fibre bundle models


## Installation

1. Open R. (If not installed, I recommend installing RStudio (free software) that can be downloaded from https://www.rstudio.com/products/rstudio/download/)
2. If not already installed, install the `devtools` package by typing `install.packages("devtools")` in the R console. This package allows you to interact with R packages hosted on GitHub (among many other things).
3. Install the `FBMcw` package by typing `devtools::install_github("GJMeijer/FBMcw")` in the R console


## Running the app

To run the included app, type `FBMcw::run_app()` in the R command line. This will open an interactive App in your default browser. This app relies on R continuing to run in the background for any computations.

This app shows an interactive version of the various fibre bundle models, capable of showing how root reinforcement predictions change when altering input parameters in real time. This app is constructed using the R package `shiny`.


## Using calculations functions

For each of the following functions, a help page can be opened by typing `?function_name` in the R console, where `function_name` is the name of the function of interest.


### Peak reinforcements (continuous models)

The `RBMcw` package contains a number of functions to calculate peak root reinforcements, assuming continuous power-law distributions of the root area ratio and root tensile strength:

- `calc_cru_wwmc`: calculate the peak reinforcement according to the Wu/Waldron model with continuous root distributions
- `calc_kku_fbmc`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMc model. Multiply the result by the results from `calc_cru_wwmc` to calculate the peak reinforcement according to the FBMc.
- `calc_kku_fbmcw`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMcw model. Multiply the result by the results from `calc_cru_wwmc` to calculate the peak reinforcement according to the FBMcw.


### Peak reinforcements (discrete models)

The package contains a number of functions to calculate peak root reinforcements, assuming root are binned in discrete classes:

- `calc_cru_wwm`: calculate the peak reinforcement according to the Wu/Waldron model
- `calc_cru_fbm`: calculate the peak root reinforcement according to the FBM model (the 'traditional' fibre bundle model)
- `calc_cru_fbmw`: calculate the peak root reinforcement according to the FBMw model (the 'traditional' fibre bundle model with added Weibull survival functions)

To discretise a continuous distribution of roots into a number of discrete classes, you can use (or use your own functions):

- `discretise_rootclasses`: takes a range of roots and splits it into a specified number of equal-width root diameter classes, assuming power-law distributions for root area ratio and root tensile strength.

### Mobilisation of root reinforcement

The following functions are available to calculat the current root reinforcement as function of strain:

- `calc_kk_fbmc`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMc model at each value of strain
- `calc_kk_fbmcw`: calculate the reduction factor in peak root reinforcement (`k''`) according to the FBMcw model at each value of strain
- `calc_cr_fbm`: calculate the current root reinforcement according to the FBM model at each value of strain (discrete root classes)
- `calc_cf_fbmw`: calculate the current root reinforcement according to the FBMw model at each value of strain (discrete root classes)

To generate suitable ranges of strains, you could use 

- `generate_strainrange`: generates a suitable range of strains to analyse the mobilisation of root reinforcement
- `generate_strainrange_fbm`: generates a suitable range of strains to analyse the mobilisation of root reinforcement for traditional fibre bundle models. It returns strains just before and after each root failure only, to minimise the amount of computations required. 
