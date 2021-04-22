#' Get range of load sharing parameters
#'
#' @description
#' This function generates a range of load sharing rules that can be
#' used for plotting
#'
#' @param betaF Load sharing values that have to be included in the range
#'   (numeric scalar)
#' @param betaFmin Minimum of the range (numeric scalar)
#' @param betaFmax Maximum of the range (numeric scalar)
#' @param n Number of discrete points used along the load sharing
#' @param round Round min and max of load sharing range to nearest multiple
#'   of `round`
#' @return a range with load sharing values betaF (numeric array)
#' @examples
#' generate_loadsharingrange(betaF = c(-4,1), betaFmin = -2, betaFmax = 2, n = 25)
#' @export

#generate range of load sharing factors
generate_loadsharingrange <- function(betaF = NA, betaFmin = -1, betaFmax = 3, n = 101, round = 0.2){
  #min and max of range
  if ((is_near(round, 0)) | is.na(round) | is.null(round)){
    betaF0 <- min(betaFmin, betaF)
    betaF1 <- max(betaFmax, betaF)
  } else {
    betaF0 <- floor(min(betaFmin, betaF)/round)*round
    betaF1 <- ceiling(max(betaFmax, betaF)/round)*round
  }
  #all values (including betaF in input), unique and sorted
  betaFout <- sort(unique(c(seq(betaF0, betaF1, l=n), betaF)))
  #return
  return(betaFout)
}
