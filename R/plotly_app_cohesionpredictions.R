#' Creates plotly for peak reinforcements bar plot
#'
#' @description
#' This function generates a plotly bar plot showing peak root
#' reinforcement according to the FBMc and FBMcw for a wide range
#' of existing load sharing models.
#'
#' @param dm dataframe with fields for existing load sharing parameters
#'   (`betaF`), the name of this load sharing rule (`loadsharing'), the
#'   normalised reinforcement according to the FBMc model (`kk_fbmc`) and
#'   the normalised reinforcement according to the FBMcw model (`kk_fbmcw`).
#' @param cru_wwmc peak root-reinforcement according to the WWMc model
#'   (numeric scalar)
#' @param nsignif number of significant digits in plotly hover labels
#'   (integer scalar)
#' @return plotly object
#' @examples
#' drules <- data.frame(
#'   loadsharing = c('test1', 'test2'),
#'   betaF = c(0.5, 1),
#'   kk_fbmc = c(0.5, 1)^2,
#'   kk_fbmcw = c(0.5, 1)^0.5
#' )
#' cru_wwmc <- 30
#' plotly_app_cohesionpredictions(drules, cru_wwmc, nsignif = 2)
#' @export

#PLOTLY function to plot cohesion predictions
plotly_app_cohesionpredictions <- function(dm, cru_wwmc, nsignif = 3){
  #hovertext
  dm$HoverTextFBMc <- with(dm, paste(
    'Model: FBMc<br>',
    'Load sharing rule: ', loadsharing, '<br>',
    '\u03b2<sub>F</sub>: ', signif(betaF, nsignif), '<br>',
    "c<sub>r,u</sub>: ", signif(kk_fbmc*cru_wwmc, nsignif), ' kPa',
    sep=''
  ))
  dm$HoverTextFBMcw <- with(dm, paste(
    'Model: FBMcw<br>',
    'Load sharing rule: ', loadsharing, '<br>',
    '\u03b2<sub>F</sub>: ', signif(betaF,nsignif), '<br>',
    "c<sub>r,u</sub>: ", signif(kk_fbmcw*cru_wwmc, nsignif), ' kPa',
    sep=''
  ))
  #plot
  p <- plotly::plot_ly(
    dm,
    x = ~loadsharing,
    y = ~kk_fbmc*cru_wwmc,
    type = 'bar',
    name = 'FBMc',
    text = ~HoverTextFBMc,
    hoverinfo = 'text'
  )
  p <- plotly::add_trace(p,
    y = ~kk_fbmcw*cru_wwmc,
    name = 'FBMcw',
    text = ~HoverTextFBMcw,
    hoverinfo = 'text'
  )
  #add layout
  p <- plotly::layout(p,
    xaxis = list(
      title = 'Load sharing rule'
    ),
    yaxis = list(
      title = 'Peak root reinforcement c<sub>r,u</sub> [kPa]'
    ),
    barmode = 'group',
    legend = list(
      title = list(
        text = 'Model'
      )
    )
  )
  return(p)
}
