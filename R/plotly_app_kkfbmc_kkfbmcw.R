#' Creates plotly for (normalised) mobilisation of FBMc and FBMcw models
#'
#' @description
#' This function generates a plotly object showing how root reinforcement
#' is mobilised as function of reference strain for both the FBMc and FBMcw
#' models. Reinforcements are normalised by the WWMc solution. Strain is
#' defined as the current tensile strain in the reference root (dr=dr0)
#' normalised by the tensile strain to failure in the reference root.
#'
#' @param dt dataframe with fields for the normalised reference strain
#'   (`eps0rel`), the normalised reinforcement according to the FBMc model
#'   (`kk_fbmc`) and the normalised reinforcement according to the FBMcw
#'   model (`kk_fbmcw`).
#' @param nsignif number of significant digits in plotly hover labels
#'   (integer scalar)
#' @return plotly object
#' @examples
#' dt <- data.frame(
#'   epsr0rel = seq(0, 1, l = 51),
#'   kk_fbmc = seq(0, 1, l = 51)^2,
#'   kk_fbmcw = seq(0, 1, l = 51)^0.5
#' )
#' plotly_app_kkfbmc_kkfbmcw(dt)
#' @export

plotly_app_kkfbmc_kkfbmcw <- function(dt, nsignif = 3){
  #hoverinfo
  dt$HoverTextFBMc <- with(dt, paste(
    'Model: FBMc<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel,nsignif), '<br>',
    "k'': ", signif(kk_fbmc,nsignif),
    sep=''
  ))
  dt$HoverTextFBMcw <- with(dt, paste(
    'Model: FBMcw<br>',
    '\u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>: ', signif(epsr0rel,nsignif), '<br>',
    "k'': ", signif(kk_fbmcw,nsignif),
    sep=''
  ))
  #create plotly object
  p <- plotly::plot_ly(
    data = dt,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~kk_fbmc,
    text = ~HoverTextFBMc,
    hoverinfo = 'text',
    name = 'FBMc'
  )
  p <- plotly::add_trace(p,
    data = dt,
    type = 'scatter',
    mode = 'lines',
    x = ~epsr0rel,
    y = ~kk_fbmcw,
    text = ~HoverTextFBMcw,
    hoverinfo = 'text',
    name = 'FBMcw'
  )
  p <- plotly::layout(p,
    xaxis = list(
      title = '(Normalised) strain in reference root \u03b5<sub>r,0</sub>/\u03b5<sub>r,u,0</sub>'
    ),
    yaxis = list(
      title = "(Normalised) reinforcement: k''=c<sub>r</sub>/c<sub>r,u,WWMc</sub>",
      range = c(0,1.05)
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
