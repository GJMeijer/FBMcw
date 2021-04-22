#' Predict cumulative root area ratio traces based on fitted power-law values
#'
#' @description
#' This function generates a root-diameter versus cumulative root area ratio
#' trace, given the inputs from a (bounded) power law fit between the root
#' diameter and cumulative root area ratio. These can be obtained using the
#' function `fit_powerlaw_phir`.
#'
#' @param drmin the minimum root diameter (numerical scalar)
#' @param drmax the maximum root diameter (numerical scalar)
#' @param phirt the total root area ratio
#' @param betaphi the power-law coefficient in the root diameter
#'   versus root area ratio fit
#' @param cumulative if TRUE, cumulative trace is returned.
#'   if FALSE, the distribution of root area ratio over all
#'   root diameters is returned (i.e. the derivative of the c
#'   cumulative trace)
#' @param dr if specified, the traces are calculated at these
#'   diameters. If not specified (default), a range is taken between
#'   `drmin` and `drmax` using `nc` points.
#' @param n number of points used to plot the trace between
#'   `drmin` <= dr <= `drmax` (used if `nc` is not specified)
#' @return dataframe with predictions. Contained fields for
#'   the root diameter (`dr`).
#'   the cumulative root area ratio (`phir_cum`), if `cumulative==T`.
#'   the distribution curve (`phir`), if `cumulative==F`.
#' @examples
#' predict_phir(1, 5, 0.01, -0.5, cumulative = TRUE)
#' predict_phir(1, 5, 0.01, -0.5, cumulative = FALSE)
#' predict_phir(1, 5, 0.01, -0.5, dr = seq(0, 10, l=25), cumulative = TRUE)
#' @export

#function for predicted cumulative phir traces
predict_phir <- function(drmin, drmax, phirt, betaphi, dr = NULL, n = 101, cumulative = TRUE){
  # dataframe with diameters
  if (is.null(dr)){
    d <- data.frame(dr = seq(drmin, drmax, l = n))
  } else {
    d <- data.frame(dr = sort(unique(c(dr, drmin, drmax, drmin-1e-6*(drmax-drmin), drmax+1e-6*(drmax-drmin)))))
  }
  #cumulative predictions
  if (cumulative == TRUE){
    if (is_near((betaphi+1),0)){
      d$phir_cum <- phirt*pmin(1,pmax(0,log(d$dr/drmin)/log(drmax/drmin)))
    } else {
      d$phir_cum <- phirt*pmin(1,pmax(0,(d$dr^(1+betaphi)-drmin^(1+betaphi))/(drmax^(1+betaphi)-drmin^(1+betaphi))))
    }
  } else {
    d$phir <- 0
    ind <- (d$dr >= drmin) & (d$dr <= drmax)
    if (is_near((betaphi+1),0)){
      d$phir[ind] <- phirt/(d$dr[ind]*log(drmax/drmin))
    } else {
      d$phir[ind] <- (phirt*d$dr[ind]^betaphi)*(1+betaphi)/(drmax^(1+betaphi)-drmin^(1+betaphi))
    }
  }
  #return
  return(d)
}
