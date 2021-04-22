#' Plotly for normalised mobilisation of FBM reinforcement in discrete classes
#'
#' @description
#' Creates a plotly object showing how reinforcement is mobilised according
#' to the FBM model with a discrete number of equal-width root diameter
#' classes. Power-law relations for the root area ratio and tensile strength
#' are assumed. The results are normalised by the Wu/Waldron (WWM) solution
#' using the same classes.
#'
#' @param drmin,drmax minumum and maximum root diameter in bundle
#'   (numeric array)
#' @param betaF Load sharing coefficient (numeric array)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric array)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric array)
#' @param kappa Weibull survival function shape parameter (numeric scalar)
#' @param nc The number of discrete, equal-width root classes to use
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param nround number of decimals used in rounding numbers shown in plot
#'   (default 2)
#' @param xlim optional max limit for x-axis
#' @param ylim optional max limit for y-axis
#' @param show_legend if TRUE, shows legend
#' @param hoverinfo_classes if TRUE, shows reinforcements for all root classes
#'   in hoverinfo information. If `skip`, this information is not shown and
#'   only information for the total reinforcement is displaced in the
#'   hoverinfo
#' @return Plotly object
#' @examples
#' plotly_stackedtrace_kk_fbm_fbmw(1, 5, 1, -0.5, -0.2, 4, 5)
#' @export

plotly_stackedtrace_kk_fbm_fbmw <- function(drmin, drmax, betaF, betat, betaphi, kappa, nc, dr0 = 1.0, nround = 2, xlim = NULL, ylim = NULL, show_legend = T, hoverinfo_classes = 'skip'){
  #split root classes
  di <- discretise_rootclasses(drmin, drmax, 1, betat, 1, betaphi, nc)

  ## WWM
  #calculate WWM solution for discrete classes
  cru_wwm <- calc_cru_wwm(di$phir, di$tru, k=1)

  ## FBM
  #generate strain range
  epsr0rel_fbm <- generate_strainrange_fbm(di$dr, di$tru, betaF, 1, includezero=T, includefailure=T, margin=1.25, failureoffset=1e-3)
  #calculate reinforcement contribution of each root for each strain step
  cr_fbm <- calc_cr_fbm(epsr0rel_fbm, di$dr, di$phir, di$tru, betaF, 1, k=1, dr0=dr0, sumoutput=F)

  ## FBMcw
  #generate strain range
  epsr0rel_fbmw <- generate_strainrange(min(di$dr), max(di$dr), betaF, betat, dr0=dr0, margin=1.25, n=101)
  #if x-axis (strain) limit specied, add points until fully filled
  if (!is.null(xlim)){
    if (xlim>max(epsr0rel_fbmw)){
      depsr0rel <- stats::median(diff(epsr0rel_fbmw))
      epsr0rel_fbmw <- c(epsr0rel_fbmw, utils::tail(seq(max(epsr0rel_fbmw),xlim,depsr0rel),-1))
    }
  }
  #calculate reinforcement contribution of each root for each strain step
  cr_fbmw <- calc_cr_fbmw(epsr0rel_fbmw, di$dr, di$phir, di$tru, betaF, 1, kappa, k=1, dr0=dr0, sumoutput=F)

  #determine stacking order - plot roots that break last first for nice visuals
  if (is_near(2+betat-betaF,0)){
    plot_order <- rev(seq(nc))
  } else {
    plot_order <- rev(order(generate_strainrange_fbm(di$dr, di$tru, betaF, 1, includezero=F, includefailure=F, outputperclass=T)))
  }
  #plot limits
  if (is.null(xlim)) {
    xlim <- max(c(epsr0rel_fbm, epsr0rel_fbmw))
  }
  if (is.null(ylim)) {
    ylim <- 1.0
  }
  #colors for plot (simulate YlGnBu from RColorBrewer)
  colo <- grDevices::colorRampPalette(c("#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84"))(nc)

  ## FBM
  #generate plotly plot
  p1 <- plotly::plot_ly()
  #add traces in loop
  for (i in plot_order){
    ind <- seq(which(cr_fbm[,i]==0)[2])
    p1 <- plotly::add_trace(
      p1,
      x = epsr0rel_fbm[ind],
      y = cr_fbm[ind,i]/cru_wwm,
      type = 'scatter',
      mode = 'lines',
      stackgroup = 'one',
      hoverinfo = hoverinfo_classes,
      line = list(
        width = 0,
        color = 'black'
      ),
      fillcolor = colo[i],
      name = paste0(round(di$drmini[i],nround), " < d<sub>r</sub> \u2264 ", round(di$drmaxi[i],nround), ' mm')    )
  }
  #add total trace
  p1 <- plotly::add_trace(
    p1,
    x = epsr0rel_fbm,
    y = rowSums(cr_fbm)/cru_wwm,
    type = 'scatter',
    mode = 'lines',
    line = list(
      width = 1.5,
      color = 'black'
    ),
    name = 'FBM',
    showlegend = FALSE
  )
  #add layout
  p1 <- plotly::layout(
    p1,
    xaxis = list(
      title = '(Normalised) strain in reference root: \u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>',
      range = c(0, xlim)
    ),
    yaxis = list(
      title = '(Normalised) reinforcement: c<sub>r,FBM</sub>/c<sub>r,u,WWM</sub>',
      range = c(0, ylim)
    ),
    hovermode = 'x_unified',
    showlegend = show_legend
  )

  #generate plotly plot
  p2 <- plotly::plot_ly()
  #add traces in loop
  for (i in plot_order){
    p2 <- plotly::add_trace(
      p2,
      x = epsr0rel_fbmw,
      y = cr_fbmw[,i]/cru_wwm,
      type = 'scatter',
      mode = 'lines',
      stackgroup = 'one',
      hoverinfo = hoverinfo_classes,
      line = list(
        width = 0,
        color = 'black'
      ),
      fillcolor = colo[i],
      showlegend = FALSE
      #name = paste0(round(di$drmini[i],nround), " < d<sub>r</sub> \u2264 ", round(di$drmaxi[i],nround), ' mm')
    )
  }
  #add total trace
  p2 <- plotly::add_trace(
    p2,
    x = epsr0rel_fbmw,
    y = rowSums(cr_fbmw)/cru_wwm,
    type = 'scatter',
    mode = 'lines',
    line = list(
     width = 1.5,
     color = 'black'
    ),
    name = 'FBMc',
    showlegend = FALSE
  )

  ## MERGE
  p <- plotly::subplot(
    p1,p2,
    nrows = 1,
    shareX = TRUE,
    shareY = TRUE
  )

  #return plot
  return(p)
}
