# FBMcw
Generic fibre bundle models for soil reinforcement by plant roots

This R package contains:
- an interactive App, interactively showing how root reinforcement predictions change when varying input parameters
- all functions required to make root reinforcement calculations according to the various fibre bundle models

## Install the package

1. Open R. (If not installed, I recommend installing RStudio (free) that can be downloaded from https://www.rstudio.com/products/rstudio/download/)
2. If not already installed, install the `devtools` package by typing `install.packages("devtools")` in the command line. This package allows you to install R packages straight from GitHub.
3. Install the `FBMcw` package by typing `devtools::install_github("GJMeijer/FBMcw")` in the command line

## Running the App

An interactive version of the model, capable of showing how root reinforcement predictions change when altering input parameters in real time, is provided using the R package `shiny`.

To run this app, type `FBMcw::run_app()` in the command line. This will open an interactive App in your default browser. This relies on R continue to run in the background to do computations.

## Using the functions

