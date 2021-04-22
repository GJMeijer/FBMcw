#' Combine root class data with individual root counts
#'
#' @description
#' This function combines root area ratio data measured by binning diameters
#' in classes (`dclass`) with data for individual roots (`dsingle`). The
#' single root data is treated like a very narrow class with with `drwidth`.
#'
#' @param dclass dataframe with root area ratio data for classes. Should
#'   contain fields for minimum (`drmin`) and maximum diameter in class
#'   (`drmax`), and the root area ratio for each class (`phir`)
#' @param dsingle dataframw with root area ratio for single roots. Should
#'   contain fields for the diameter (`dr`) and the root area ratio for
#'   roots with this diameter (`phir`).
#' @param drwidth the width of the assumed class for individual root
#'   diameters (should be a very small value, default 1e-12)
#' @return dataframe with all data. Contain fields for minimum (`drmin`)
#'   and maximum diameter in class (`drmax`), and the root area ratio for
#'   each class (`phir`)
#' @examples
#' dclass <- data.frame(
#'   drmin = c(1, 2, 3, 4),
#'   drmax = c(2, 3, 3.5, 5),
#'   phir = c(0.1, 0.2, 0.4, 0.7)
#' )
#' dsingle <- data.frame(
#'   dr = c(3.3, 5.5),
#'   phir = c(0.05, 0.05)
#' )
#' join_datatypes_phir(dclass, dsingle)
#' @export

join_datatypes_phir <- function(dclass, dsingle, drwidth = 1e-12){
  #treat single roots are very narrow classes
  if (!is.null(dsingle)){
    dsingle$drmin <- dsingle$dr - 0.5*drwidth
    dsingle$drmax <- dsingle$dr + 0.5*drwidth
  }
  #bind together
  d <- rbind(
    dclass[,c('drmin','drmax','phir')],
    dsingle[,c('drmin','drmax','phir')]
  )
  #return
  return(d)
}
