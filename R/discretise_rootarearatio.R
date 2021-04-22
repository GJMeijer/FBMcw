#' Discretise root area ratio over a number of discrete diameter classes
#'
#' @description
#' This function splits a root diameter range into a number of equal-width
#' root diameter classes, and calculates the fraction on the total root
#' area ratio that should be assigned to each class. Function is superseded
#' by `discretise_rootclasses`.
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric scalars)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)#' @param phir root area ratio of roots in class
#'   (numeric arra, size m)
#' @param nc The number of discrete, equal-width root classes to use
#' @return a dataframe containing the fields `dr`, the diameter in the middle
#'   of each class, and `phirphirt`, the fraction of the total root area
#'   ratio that should be assigned to each class
#' @examples
#' discretise_rootararatio(1, 5, -0.5, 10)
#' @export

#discretize root area ratio over a number of classes
discretise_rootararatio <- function(drmin, drmax, betaphi, nc){
  #if drmin = drmax --> return single class with all rar
  if (is_near(drmin, drmax)){
    dr <- data.frame(
      dr = drmin,
      phirphirt = 1.0
    )
  } else {
    #create root diameter classes
    ds <- data.frame(
      dr1 = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(0.0*1/nc)),  #smallest diameter in class
      dr2 = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(1.0*1/nc)),  #larger diameter in class
      dr  = drmin + (drmax-drmin)*(seq(0,1-1/nc,l=nc)+(0.5*1/nc))   #middle diameter in class
    )
    #relative root volume per class
    if (is_near(betaphi+1,0)) {
      ds$phirphirt <- with(ds,
                           log(dr2/dr1) /
                             log(drmax/drmin)
      )
    } else {
      ds$phirphirt <- with(ds,
                           (dr2^(1+betaphi)-dr1^(1+betaphi)) /
                             (drmax^(1+betaphi)-drmin^(1+betaphi))
      )
    }
    #return
    return(ds[,c('dr','phirphirt')])
  }
}
