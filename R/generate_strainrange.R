#' Generate suitable range of strains for plotting FBMc/FBMcw models
#'
#' @description
#' This function generates range of strains (defined as the strain in the
#' reference root, normalised by the strain to peak in the reference root)
#' that can be used for plotting the mobilisation of root reinforcement
#' This normalised strain is referred to as `epsr0rel`
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric scalars)
#' @param betaF load sharing coefficient (numeric scalars)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric scalars)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param margin Multiplier to generate a larger strain range than required
#'   for the FBMc model (numeric scalar, default 1.0)
#' @param n Number of discrete points used along the strain range
#' @return array with strain of (normalised strain) `epsr0rel`
#' @examples
#' generate_strainrange(1, 5, 1, -0.5, margin = 1.5, n = 25)
#' @export

#generate suitable strain range
generate_strainrange <- function(drmin, drmax, betaF, betat, dr0 = 1.0, margin = 1.0, n = 101){
  #reference strains to get to failure in thinnest and thickest root
  epsr0relmin <- (drmin/dr0)^(2-betaF+betat)
  epsr0relmax <- (drmax/dr0)^(2-betaF+betat)
  #create range
  epsr0rel <- seq(0,margin*max(c(epsr0relmin,epsr0relmax)),l=n)
  #add some points - strains at breakage of min and max diameter root in FBMc
  epsr0rel <- sort(unique(c(epsr0rel,epsr0relmin,epsr0relmax)))
  #return range
  return(epsr0rel)
}
