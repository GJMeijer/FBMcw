#' Creates plotly for mobilisation of root reinforcement by all models
#'
#' @description
#' This function generates a plotly bar plot showing how root
#' reinforcement mobilises according to the FBM, FBMw, FBMc and FBMcw
#' models, all in a single graph
#'
#' @param dFBMc dataframe with fields for the normalised reference strain
#'   (`eps0rel`), the normalised reinforcement according to the FBMc model
#'   (`kk_fbmc`) and the normalised reinforcement according to the FBMcw
#'   model (`kk_fbmcw`).
#' @param cru_wwmc peak root-reinforcement according to the WWMc model
#'   (numeric scalar)
#' @param dFBM dataframe with FBM model predictions. Contains fields
#'   for the (normalised) reference strain `epsr0rel` and the corresponding
#'   reinforcement `cr_fbm`
#' @param dFBMw dataframe with FBMw model predictions. Contains fields
#'   for the (normalised) reference strain `epsr0rel` and the corresponding
#'   reinforcement `cr_fbmw`
#' @param nsignif number of significant digits in plotly hover labels
#'   (integer scalar)
#' @return plotly object
#' @examples
#' dFBMc <- data.frame(   #FBMc and FBMcw predictions
#'   epsr0rel = seq(0, 1, l = 51),
#'   kk_fbmc = seq(0, 1, l = 51)^2,
#'   kk_fbmcw = seq(0, 1, l = 51)^0.5
#' )
#' cru_wwmc <- 30      #WWMc prediction
#' dFBM <- data.frame(   #FBM predictions
#'   epsr0rel = seq(0, 1, l = 6),
#'   cr_fbm = 20 * seq(0, 1, l = 6)^2
#' )
#' dFBMw <- data.frame(   #FBMw predictions
#'   epsr0rel = seq(0, 1, l = 6),
#'   cr_fbmw = 15 * seq(0, 1, l = 6)^0.6
#' )
#' plotly_app_alltraces(dFBMc, cru_wwmc, dFBM, dFBMw, nsignif = 2)
#' @export

#PLOTLY function to plot strain traces
plotly_app_alltraces <- function(dFBMc, cru_wwmc, dFBM, dFBMw, nsignif = 3){
  #hoverinfo
  dFBMc$HoverTextFBMc <- with(dFBMc, paste(
    'Model: FBMc<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel, nsignif), '<br>',
    "c<sub>r</sub>': ", signif(kk_fbmc*cru_wwmc, nsignif), ' kPa',
    sep=''
  ))
  dFBMc$HoverTextFBMcw <- with(dFBMc, paste(
    'Model: FBMcw<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel, nsignif), '<br>',
    "c<sub>r</sub>': ", signif(kk_fbmcw*cru_wwmc, nsignif), ' kPa',
    sep=''
  ))
  dFBM$HoverText <- with(dFBM, paste(
    'Model: FBM<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel, nsignif), '<br>',
    "c<sub>r</sub>': ", signif(cr_fbm, nsignif), ' kPa',
    sep=''
  ))
  dFBMw$HoverText <- with(dFBMw, paste(
    'Model: FBMw<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel, nsignif), '<br>',
    "c<sub>r</sub>': ", signif(cr_fbmw, nsignif), ' kPa',
    sep=''
  ))

  #create plot
  p <- plotly::plot_ly(
    data = dFBMc,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~kk_fbmc * cru_wwmc,
    name = 'FBMc',
    text = ~HoverTextFBMc,
    hoverinfo = 'text'
  )
  p <- plotly::add_trace(p,
    data = dFBMc,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~kk_fbmcw * cru_wwmc,
    name = 'FBMcw',
    text = ~HoverTextFBMcw,
    hoverinfo = 'text'
  )
  p <- plotly::add_trace(p,
    data = dFBM,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~cr_fbm,
    name = 'FBM',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  p <- plotly::add_trace(p,
    data = dFBMw,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~cr_fbmw,
    name = 'FBMw',
    text = ~HoverText,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(p,
    xaxis = list(
      title = '(Normalised) strain in reference root \u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>'
    ),
    yaxis = list(
      title = 'Root reinforcement c<sub>r</sub> [kPa]'
    ),
    legend = list(
      title = list(
        text = 'Model'
      )
    )
  )
  #return
  return(p)
}
