#' Create a diameter versus cumulative root area ratio trace
#'
#' @description
#' This function creates a trace for root diameter versus cumulative root
#' area ratio, based on binned root diameter/root area ratio input
#'
#' @param dclass dataframe with root area ratio data for classes. Should
#'   contain fields for minimum (`drmin`) and maximum diameter in class
#'   (`drmax`), and the root area ratio for each class (`phir`)
#' @return dataframe with cumulative traces. Contain fields for root
#'   diameter (`dr`) and cumulative root area ratio (`phir_cum`)
#' @examples
#' dclass <- data.frame(
#'   drmin = c(1, 2, 3, 4),
#'   drmax = c(2, 3, 3.5, 5),
#'   phir = c(0.1, 0.2, 0.4, 0.7)
#' )
#' create_cumulative_phir(dclass)
#' @export

create_cumulative_phir <- function(dclass){
  #unique diameter values
  drunique <- sort(unique(c(dclass$drmin, dclass$drmax)))
  #interpolate each class
  phir_cum <- rowSums(apply(dclass[,c('drmin','drmax','phir')], 1, function(x) stats::approx(c(x[1],x[2]), c(0,x[3]), xout=drunique, yleft=0, yright=x[3])$y))
  #return dataframe
  return(
    data.frame(
      dr = drunique,
      phir_cum = phir_cum
    )
  )
}
