#' Fit a bounded power-law trace to diameter versus cumulative root area ratio
#'
#' @description
#' This function fits a power-law curve to root diameter versus cumulative
#' root area ratio data. This power law is bounded by a minimum (`drmin`)
#' and maximum (`drmax`) root diameter.
#'
#' @param dcum dataframe with fields for the root diameter (`dr') and
#' cumulative root area ratio (`phir_cum`).
#' @param drmin an user-defined lower limit to the fitted
#'   root diameter range. Should be larger than zero. Default 1e-12
#' @param drmax an user-defined upper limit to the fitted
#'   root diameter range. Should be larger than zero. Default infinity
#' @param betaphi_initial_guess if TRUE, betaphi is first approximated
#'   by fixing `drmin` and `drmax` between the known values in the input
#'   this best fit is subsequently used in the 'real' fit. This should
#'   increase the likelyhood of obtaining a suitable fitting result
#' @return dataframe with fitting parameters. Returns:
#'   the total root area ratio (`phirt`)
#'   the best-fit power-law coefficient (`betaphi`)
#'   the best-fit minimum root diameter (`drmin`)
#'   the best-fit maximum root diameter (`drmax`)
#'   the Kolmogorov-Smirnov distance of the fit (`KS`)
#' @examples
#' dcum <- data.frame(
#'   dr = c(1, 2, 3, 4, 5),
#'   phir_cum = c(0, 0.3, 0.5, 0.9, 0.95)
#' )
#' fit_powerlaw_phir(dcum)
#' @export

fit_powerlaw_phir <- function(dcum, drmin = 1e-12, drmax = Inf, betaphi_initial_guess = T){
  #if any diameters <= 0, set small value (otherwise problems with power-laws)
  dcum$dr[dcum$dr<=0] <- 1e-90
  #total root area ratio
  phirt <- max(dcum$phir_cum)
  #fitting weight for each point: 0.5*distance between previous and next
  dcum$weight <- with(dcum, 0.5*c(0,diff(phir_cum)) + 0.5*c(diff(phir_cum),0))
  #power-law fitting function (log-transform diameters first for stability)
  fit_powerlaw_lnd <- function(lnd, betaphi, lnd1, lnd2){
    if ((betaphi+1)==0){
      return(pmin(1,pmax(0,(lnd-lnd1)/(lnd2-lnd1))))
    } else {
      return(pmin(1,pmax(0,(exp((1+betaphi)*lnd)-exp((1+betaphi)*lnd1))/(exp((1+betaphi)*lnd2)-exp((1+betaphi)*lnd1)))))
    }
  }
  #initial guess for betaphi - fixed drmin and drmax --> for better convergence
  if (betaphi_initial_guess == T){
    ftln0 <- stats::nls(
      (phir_cum/phirt) ~ fit_powerlaw_lnd(log(dr), betaphi, log(min(dr)), log(max(dr))),
      data = dcum,
      start = list(betaphi=0),
      weight = dcum$weight
    )
    betaphi0 <- stats::coef(ftln0)[1]
  } else {
    betaphi0 <- 0
  }
  #best fit for betaphi, drmin and drmax
  ftln1 <- stats::nls(
    phir_cum/phirt ~ fit_powerlaw_lnd(log(dr), betaphi, lnd1, lnd2),
    data = dcum,
    start = list(betaphi = betaphi0, lnd1 = log(min(dcum$dr)), lnd2 = log(max(dcum$dr))),
    weight = dcum$weight,
    lower = c(-Inf, log(drmin), log(drmin)),
    upper = c(Inf, log(drmax), log(drmax)),
    algorithm = 'port'
  )
  #Kolmogorov-Smirnov distance
  KS <- max(abs(stats::predict(ftln1) - (dcum$phir_cum/phirt)))
  #dataframe with results
  dat <- data.frame(
    phirt = phirt,
    betaphi = as.numeric(stats::coef(ftln1)[1]),
    drmin = as.numeric(exp(stats::coef(ftln1)[2])),
    drmax = as.numeric(exp(stats::coef(ftln1)[3])),
    KS = KS
  )
  #return
  return(dat)
}
