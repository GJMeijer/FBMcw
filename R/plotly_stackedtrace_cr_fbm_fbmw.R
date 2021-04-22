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
#'   (numeric scalar)
#' @param betaF Load sharing coefficient (numeric scalar)
#' @param tru0 tensile strength of root with reference diameter (in kPa)
#' @param betat Power law coefficient for root diameter-root tensile strength
#'   fit (numeric scalar)
#' @param phirt Total root area ratio in bundle (numeric scalar)
#' @param betaphi Power law coefficient for root diameter-root area ratio
#'   fit (numeric scalar)
#' @param kappa Weibull survival function shape parameter (numeric scalar)
#' @param nc The number of discrete, equal-width root classes to use
#' @param k Wu/Waldron factor accounting for root orientations at failure
#'   (numeric scalar, default 1.2)
#' @param dr0 Reference diameter (numeric scalar, default 1.0)
#' @param nround number of decimals used in rounding diameter classes in plot
#'   (default 2)
#' @param xlim optional max limit for x-axis
#' @param ylim optional max limit for y-axis
#' @param show_legend if TRUE, shows legend
#' @param hoverinfo_classes if TRUE, shows reinforcements for all root classes
#'   in hoverinfo information. If `skip`, this information is not shown and
#'   only information for the total reinforcement is displaced in the
#'   hoverinfo
#' @param nsignif number of significant digits in plotly hover labels
#'   (integer scalar)
#' @param plot_continuous if TRUE, also plot continuous solutions
#' @param plot_classes if TRUE, the effect contibution of individual classes is added
#' @return Plotly object
#' @examples
#' plotly_stackedtrace_cr_fbm_fbmw(1, 5, 1, 10e3, -0.5, 0.01, -0.2, 4, 5)
#' @export

plotly_stackedtrace_cr_fbm_fbmw <- function(drmin, drmax, betaF, tru0, betat, phirt, betaphi, kappa, nc, k = 1.2, dr0 = 1.0, nround = 2, xlim = NULL, ylim = NULL, show_legend = T, hoverinfo_classes = 'skip', nsignif = 3, plot_continuous = TRUE, plot_classes = TRUE){
  #split root classes
  di <- discretise_rootclasses(drmin, drmax, tru0, betat, phirt, betaphi, nc)

  ## FBM
  #generate strain range
  epsr0rel_fbm <- generate_strainrange_fbm(di$dr, di$tru, betaF, tru0, includezero=T, includefailure=T, margin=1.25, failureoffset=1e-3)
  #calculate reinforcement contribution of each root for each strain step
  cr_fbm <- calc_cr_fbm(epsr0rel_fbm, di$dr, di$phir, di$tru, betaF, tru0, k=k, dr0=dr0, sumoutput=F)
  #hoverinfo
  HoverTextFBM <- paste(
    'Model: FBM<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel_fbm, nsignif), '<br>',
    "c<sub>r</sub>: ", signif(rowSums(cr_fbm), nsignif), ' kPa',
    sep=''
  )

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
  cr_fbmw <- calc_cr_fbmw(epsr0rel_fbmw, di$dr, di$phir, di$tru, betaF, tru0, kappa, k=k, dr0=dr0, sumoutput=F)
  #hoverinfo
  HoverTextFBMw <- paste(
    'Model: FBMw<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel_fbmw, nsignif), '<br>',
    "c<sub>r</sub>: ", signif(rowSums(cr_fbmw), nsignif), ' kPa',
    sep=''
  )

  ## CONTINUOUS SOLUTIONS
  if (plot_continuous == T){
    #calculate
    kk_fbmc <- calc_kk_fbmc(epsr0rel_fbmw, drmin, drmax, betaF, betat, betaphi, dr0 = dr0)
    kk_fbmcw <- calc_kk_fbmcw(epsr0rel_fbmw, drmin, drmax, betaF, betat, betaphi, kappa, dr0 = dr0)
    phir0 <- calc_phir0(phirt, drmin, drmax, betaphi, dr0=dr0)
    cru_wwmc <- calc_cru_wwmc(drmin, drmax, tru0, betat, phir0, betaphi, k = k, dr0 = dr0)
    #hovertext
    HoverTextFBMc <- paste(
      'Model: FBMc<br>',
      '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel_fbmw, nsignif), '<br>',
      "c<sub>r</sub>: ", signif(kk_fbmc*cru_wwmc, nsignif), ' kPa',
      sep=''
    )
    HoverTextFBMcw <- paste(
      'Model: FBMcw<br>',
      '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel_fbmw, nsignif), '<br>',
      "c<sub>r</sub>: ", signif(kk_fbmc*cru_wwmc, nsignif), ' kPa',
      sep=''
    )
  }

  #determine stacking order - plot roots that break last first for nice visual
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
    ylim <- NULL
  }
  #colors for plot (simulate YlGnBu from RColorBrewer)
  colo <- grDevices::colorRampPalette(c("#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#225EA8","#0C2C84"))(nc)

  ## FBM
  #generate plotly plot
  p1 <- plotly::plot_ly()
  #add traces in loop
  if (plot_classes == TRUE){
    for (i in plot_order){
      ind <- seq(which(cr_fbm[,i]==0)[2])
      p1 <- plotly::add_trace(
        p1,
        x = epsr0rel_fbm[ind],
        y = cr_fbm[ind,i],
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
  }
  #add total trace
  p1 <- plotly::add_trace(
    p1,
    x = epsr0rel_fbm,
    y = rowSums(cr_fbm),
    type = 'scatter',
    mode = 'lines',
    line = list(
      width = 1.5,
      color = 'black'
    ),
    name = 'FBM',
    #showlegend = FALSE,
    text = HoverTextFBM,
    hoverinfo = 'text'
  )
  #add total continuous trace
  if (plot_continuous == T){
    p1 <- plotly::add_trace(
      p1,
      x = epsr0rel_fbmw,
      y = kk_fbmc*cru_wwmc,
      type = 'scatter',
      mode = 'lines',
      line = list(
        width = 1.5,
        color = 'black',
        dash = 'dash'
      ),
      name = 'FBMc',
      #showlegend = FALSE,
      text = HoverTextFBMc,
      hoverinfo = 'text'
    )
  }
  #add layout
  p1 <- plotly::layout(
    p1,
    xaxis = list(
      title = '(Normalised) strain in reference root: \u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>',
      range = c(0, xlim)
    ),
    yaxis = list(
      title = 'Root reinforcement: c<sub>r</sub> [kPa]',
      range = c(0, ylim)
    ),
    hovermode = 'x_unified',
    showlegend = show_legend
  )

  #generate plotly plot
  p2 <- plotly::plot_ly()
  #add traces in loop
  if (plot_classes == TRUE){
    for (i in plot_order){
      p2 <- plotly::add_trace(
        p2,
        x = epsr0rel_fbmw,
        y = cr_fbmw[,i],
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
  }
  #add total trace
  p2 <- plotly::add_trace(
    p2,
    x = epsr0rel_fbmw,
    y = rowSums(cr_fbmw),
    type = 'scatter',
    mode = 'lines',
    line = list(
     width = 1.5,
     color = 'red'
    ),
    name = 'FBMw',
    #showlegend = FALSE,
    text = HoverTextFBMw,
    hoverinfo = 'text'
  )
  #add total continuous trace
  if (plot_continuous == T){
    p2 <- plotly::add_trace(
      p2,
      x = epsr0rel_fbmw,
      y = kk_fbmcw*cru_wwmc,
      type = 'scatter',
      mode = 'lines',
      line = list(
        width = 1.5,
        color = 'red',
        dash = 'dash'
      ),
      name = 'FBMcw',
      #showlegend = FALSE,
      text = HoverTextFBMc,
      hoverinfo = 'text'
    )
  }

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
