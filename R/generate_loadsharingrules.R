#' Get load sharing parameters for common fibre bundle models
#'
#' @description
#' This function generates a list of load sharing rules associated
#' with common existing root reinforcement models. Rules are generated
#' based on the optional input parameters provided
#'
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric scalar)
#' @param betaE Power law coefficient for root diameter-root stiffness
#'   fit (numeric scalar)
#' @param betaL Power law coefficient for root diameter-root length
#'   fit (numeric scalar)
#' @param betaeps Power law coefficient for root diameter-root strain to peak
#'   fit. This value overrules `betaE` if provided (numeric scalar)
#' @param betaF Additional user-defined load sharing rules (numerical array)
#' @return dataframe with fields `loadsharingrule` and `betaF`, containing the name of the
#'   load sharing rule and the value of the load sharing coefficient respectively.
#' @examples
#' generate_loadsharingrules(betat = -0.5, betaE = 0, betaL = 0.7, betaF = 1.0)
#' @export

#Load sharing values
generate_loadsharingrules <- function(betat=NULL, betaE=NULL, betaL=NULL, betaeps=NULL, betaF=NULL){
  #craete empty dataframe
  d <- data.frame(loadsharing = character(), betaF = numeric())
  #add standard models - WWM
  if (!is.null(betat)){
    d <- data.frame(
      loadsharing = c('WWM'),
      betaF = c(2 + betat)
    )
  }
  #add standard models
  d <- rbind(
    d,
    data.frame(
      loadsharing = c('FBM \u03b2<sub>F</sub>=0', 'FBM \u03b2<sub>F</sub>=1', 'FBM \u03b2<sub>F</sub>=2'),
      betaF = c(0, 1, 2)
    )
  )
  #models that require stiffness
  if (!(is.null(betaeps) & is.null(betaE))){
    #betaE or betaeps? betaeps overrules betaE
    if (!is.null(betaeps)){
      betaE <- betat - betaeps
    }
    #add RBMw - if betaL is given
    if (!is.null(betaL)){
      d3 <- data.frame(
        loadsharing = 'RBMw',
        betaF = c(2 + betaE - betaL)
      )
      d <- rbind(d, d3)
    }
    #calulate values - energy models and waldron
    d2 <- data.frame(
      loadsharing = c('FBM-WN', 'FBM-WDia', 'FBM-WS', 'Waldron'),
      betaF = c(2.0+0.5*betaE, 2.5+0.5*betaE, 3.0+0.5*betaE, 1.5+0.5*betaE)
    )
    d <- rbind(d,d2)
  }
  #add user-defined values if needed
  if (!is.null(betaF)){
    if (length(betaF)==1){
      d1 <- data.frame(
        loadsharing = 'User input',
        betaF = betaF
      )
    } else {
      d1 <- data.frame(
        loadsharing = paste('User input ',seq(length(betaF)),sep=''),
        betaF = betaF
      )
    }
    d <- rbind(d,d1)
  }
  #return
  return(d)
}
