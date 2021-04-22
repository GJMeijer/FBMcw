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
#' plotly_stackedtrace_kk_fbm(1, 5, 1, -0.5, -0.2, 5)
#' @export

plotly_stackedtrace_kk_fbm <- function(drmin, drmax, betaF, betat, betaphi, nc, dr0 = 1.0, nround = 2, xlim = NULL, ylim = NULL, show_legend = T, hoverinfo_classes = 'skip'){
  #split root classes
  di <- discretise_rootclasses(drmin, drmax, 1, betat, 1, betaphi, nc)
  #generate strain range
  epsr0rel <- generate_strainrange_fbm(di$dr, di$tru, betaF, 1, includezero=T, includefailure=T, margin=1.25, failureoffset=1e-3)
  #calculate reinforcement contribution of each root for each strain step
  cr_fbm <- calc_cr_fbm(epsr0rel, di$dr, di$phir, di$tru, betaF, 1, k=1, dr0=dr0, sumoutput=F)
  #calculate WWM solution for discrete classes
  cru_wwm <- calc_cru_wwm(di$phir, di$tru, k=1)
  #determine stacking order - plot roots that break last first for nice visuals
  if (is_near(2+betat-betaF,0)){
    plot_order <- rev(seq(nc))
  } else {
    plot_order <- rev(order(generate_strainrange_fbm(di$dr, di$tru, betaF, 1, includezero=F, includefailure=F, outputperclass=T)))
  }
  #colors for plot (simulate YlGnBu from RColorBrewer)
  cols <- grDevices::colorRampPalette(c("#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84"))(nc)
  #plot limits
  if (is.null(xlim)) {
    xlim <- max(epsr0rel)
  }
  if (is.null(ylim)) {
    ylim <- 1.0
  }
  #generate plotly plot
  p <- plotly::plot_ly()
  #add traces in loop
  for (i in plot_order){
    ind <- seq(which(cr_fbm[,i]==0)[2])
    p <- plotly::add_trace(p,
      x = epsr0rel[ind],
      y = cr_fbm[ind,i]/cru_wwm,
      type = 'scatter',
      mode = 'lines',
      stackgroup = 'one',
      hoverinfo = hoverinfo_classes,
      line = list(
        width = 0,
        color = 'black'
      ),
      fillcolor = cols[i],
      name = paste0(round(di$drmini[i],nround), " < d<sub>r</sub> \u2264 ", round(di$drmaxi[i],nround), ' mm')    )
  }
  #add total trace
  p <- plotly::add_trace(p,
    x = epsr0rel,
    y = rowSums(cr_fbm)/cru_wwm,
    type = 'scatter',
    mode = 'lines',
    line = list(
      width = 1.5,
      color = 'black'
    ),
    name = 'Total reinforcement'
  )
  #add layout
  p <- plotly::layout(p,
    xaxis = list(
      title = '(Normalised) strain in reference root: \u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>',
      range = c(0, xlim)
    ),
    yaxis = list(
      #title = "Reinforcement reduction coefficient k''",
      title = '(Normalised) reinforcement: c<sub>r,FBM</sub>/c<sub>r,u,WWM</sub>',
      range = c(0, ylim)
    ),
    hovermode = 'x_unified',
    showlegend = show_legend
  )
  #return plot
  return(p)
}
