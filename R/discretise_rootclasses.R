#' Discretise root area ratio over a number of discrete diameter classes
#'
#' @description
#' This function splits a root diameter range into a number of equal-width
#' root diameter classes, and calculates the root area ratio that should
#' be assigned to each class as well as the tensile strength. Power law
#' distributions for root area ratio and tensile strength are assumed.
#' The representative diameter for each class is chosen halfway the range of
#' diameters in the class.
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric scalars)
#' @param phirt Total root area ratio (numeric scalar)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param tru0 Tensile strength of root with reference diameter (numeric array)
#' @param betat Power coefficient for tensile strength (scalar)
#' @param nc The number of discrete, equal-width root classes to use
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @return a dataframe containing the fields `dr`, the diameter in the middle
#'   of each class, `phir`, the root area ratio that should be assigned to
#'   each class, and `tru`, the tensile strength assigned to each class. It
#'   also return `drmini` and `drmaxi`, the lower and upper diameter in each class
#' @examples
#' discretise_rootclasses(1, 5, 10e3, -0.5, 0.01, -0.5, 10)
#' @export

#discretize root area ratio over a number of classes
discretise_rootclasses <- function(drmin, drmax, tru0, betat, phirt, betaphi, nc, dr0 = 1){
  #if drmin = drmax --> return single class with all rar
  if (is_near(drmin, drmax)){
    dr <- data.frame(
      dr = 0.5*(drmin+drmax),
      drmini = drmin,
      drmaxi = drmax,
      phir = phirt,
      tru = tru0 * (drmin/dr0)^betat
    )
  } else {
    #create root diameter classes
    ds <- data.frame(
      dr  = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(0.5*1/nc)),  #middle diameter in class
      drmini = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(0.0*1/nc)),  #smallest diameter in class
      drmaxi = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(1.0*1/nc))   #larger diameter in class
    )
    #relative root volume per class
    if (is_near(betaphi+1,0)) {
      ds$phir <- with(ds, phirt*(log(drmaxi/drmini)/log(drmax/drmin))
      )
    } else {
      ds$phir <- with(ds, phirt*((drmaxi^(1+betaphi)-drmini^(1+betaphi))/(drmax^(1+betaphi)-drmin^(1+betaphi))))
    }
    #tensile strength
    ds$tru <- with(ds, tru0 * (dr/dr0)^betat)
    #return
    return(ds)
  }
}
