#' Generate range of strains for FBM model (discrete root classes)
#'
#' @description
#' This function generates a range of strains to be used for FBM calculations,
#' assuming discrete root classes. Strain is given as the (normalised)
#' strain in the reference root (tensile strain/tensile strain to peak). It
#' calculates the strains at failure of each root class, and optionally adds
#' a zero-strain points and the strain points just after root failures (to draw
#' nice 'sawtooth' plots).
#'
#' @param dr representative diameter of root class (numeric array, size m)
#' @param tru tensile strenght of roots in root class (numeric array, size m)
#' @param betaF load sharing parameter (numeric scalar)
#' @param tru0 tensile strength of root with reference diameter
#' @param includezero If TRUE, includes a zero-strain point
#' @param includefailure if TRUE,  includes a extra strain point
#'   just after failure in each class. The offset is governed by the
#'   value of `failureoffset`
#' @param failureoffset The strain offset for strain points just after root
#'   failure. This offset is equal to the value of `includefailure`
#'   multiplied by strain to get the last root to fail
#' @param margin if specified, add an extra point at `margin*max(epsr0rel)'
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param outputperclass if TRUE, return the values of breakage strain for each input class
#' @return numeric array with (reference) strains `epsr0rel`
#' @examples
#' generate_strainrange_fbm(seq(1,5,l=10), 10e3*seq(1.5,1,l=10), 1, 10e3)
#' @export

generate_strainrange_fbm <- function(dr, tru, betaF, tru0, includezero = T, includefailure = T, failureoffset = 1e-6, margin = NULL, dr0 = 1, outputperclass = F){
  #generate strains at failure
  epsr0relu <- tru/tru0*(dr/dr0)^(2-betaF)
  #add failure offset if required
  if (includefailure == T){
    epsr0relu2 <- epsr0relu + failureoffset*max(epsr0relu)
  } else {
    epsr0relu2 <- NULL
  }
  #add zero point if required
  if (includezero == T){
    epsr0relu3 <- 0
  } else {
    epsr0relu3 <- NULL
  }
  #add margin if required
  if (!is.null(margin)){
    epsr0relu4 <- margin*max(epsr0relu)
  } else {
    epsr0relu4 <- NULL
  }
  #append all and sort
  if (outputperclass == T){
    epsr0rel_all <- epsr0relu
  } else {
    epsr0rel_all <- sort(unique(c(epsr0relu,epsr0relu2,epsr0relu3,epsr0relu4)))
  }
  #return
  return(epsr0rel_all)
}
