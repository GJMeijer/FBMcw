#' Calculate root reinforcement at current strain according to FBM model
#'
#' @description
#' This function calculates the current root reinforcement according to the
#' FBM, assuming discrete root classes, given the current level of (normalised)
#' strain in the reference root
#'
#' @param epsr0rel tensile strain in reference root, normalised by the tensile
#'   strain to peak in this root (numeric array, size n)
#' @param dr representative diameter of root class (numeric array, size m)
#' @param phir root area ratio of roots in class (numeric array, size m)
#' @param tru tensile strength of roots in class (numeric array, size m)
#' @param betaF load sharing parameter (numeric scalar)
#' @param tru0 Tensile strength of root with reference diameter (numeric array)
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param sumoutput If TRUE, a total reinforcement is outputted for every
#'   strain step. If FALSE, a reinforcement matrix is outputted
#'   (rows: strain step, columns: classes)
#' @return numeric array with root reinforcement preductions `cr_fbm`
#' @examples
#' calc_cr_fbm(seq(0,2.5,l=101), seq(1,4), rep(0.01,4), 10e3*seq(4)^-0.5, 1, 10e3)
#' @export

calc_cr_fbm <- function(epsr0rel, dr, phir, tru, betaF, tru0, k = 1.2, dr0 = 1, sumoutput = TRUE){
  #number of strain steps and root classes
  n <- length(epsr0rel)
  m <- length(dr)
  #reference strain to failure in each class
  epsr0relu <- tru/tru0*(dr/dr0)^(2-betaF)
  #matrices - for each strain (row) and class (column)
  epsr0rel_m <- matrix(epsr0rel, nrow=n, ncol=m, byrow = FALSE)
  epsr0relu_m <- matrix(epsr0relu, nrow=n, ncol=m, byrow = TRUE)
  #breakage parameter
  fb_m <- (epsr0rel_m<=epsr0relu_m)
  #stiffness of the reinforcement per class
  dcrdeps_m <- matrix(k*phir*tru0*(dr/dr0)^(betaF-2), nrow=n, ncol=m, byrow = TRUE)
  #calculate current reinforcement
  if (sumoutput == T){
    cr_fbm <- rowSums(dcrdeps_m*epsr0rel_m*fb_m)
  } else {
    cr_fbm <- dcrdeps_m*epsr0rel_m*fb_m
  }
  #return
  return(cr_fbm)
}
