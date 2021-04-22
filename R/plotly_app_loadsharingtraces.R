#' Creates plotly for peak reinforcements as function of load sharing
#'
#' @description
#' This function generates a plotly object showing how peak root reinforcement
#' changes as function of the load sharing parameter `betaF`. It plots results
#' for a range of `betaF` for both the FBMc and FBMcw models. Load sharing
#' assumptions of existing load sharing rules are highlighted
#'
#' @param dt dataframe with fields for a range of load sharing parameters
#'   (`betaF`), the normalised reinforcement according to the FBMc model
#'   (`kk_fbmc`) and the normalised reinforcement according to the FBMcw
#'   model (`kk_fbmcw`).
#' @param dp dataframe with fields for existing load sharing parameters
#'   (`betaF`), the name of this load sharing rule (`loadsharing'), the
#'   normalised reinforcement according to the FBMc model (`kk_fbmc`) and
#'   the normalised reinforcement according to the FBMcw model (`kk_fbmcw`).
#' @param nsignif number of significant digits in plotly hover labels
#'   (integer scalar)
#' @return plotly object
#' @examples
#' dtrace <- data.frame(
#'   betaF = seq(0, 1, l = 51),
#'   kk_fbmc = seq(0, 1, l = 51)^2,
#'   kk_fbmcw = seq(0, 1, l = 51)^0.5
#' )
#' drules <- data.frame(
#'   loadsharing = c('test1', 'test2'),
#'   betaF = c(0.5, 0.8),
#'   kk_fbmc = c(0.5, 0.8)^2,
#'   kk_fbmcw = c(0.5, 0.8)^0.5
#' )
#' plotly_app_loadsharingtraces(dtrace, drules, nsignif = 2)
#' @export

plotly_app_loadsharingtraces <- function(dt, dp, nsignif = 3){
  #create plotly object
  p <- plotly::plot_ly(
    data = dt,
    type = 'scatter',
    mode = 'lines',
    x = ~betaF,
    y = ~kk_fbmc,
    name = 'FBMc',
    hoverinfo = 'skip',
    showlegend = T
  )
  p <- plotly::add_trace(p,
    data = dt,
    type = 'scatter',
    mode = 'lines',
    x = ~betaF,
    y = ~kk_fbmcw,
    name = 'FBMcw',
    hoverinfo = 'skip',
    showlegend = T
  )
  #add layout
  p <- plotly::layout(p,
    xaxis = list(
      title = 'Load sharing parameter \u03b2<sub>F</sub>'
    ),
    yaxis = list(
      title = "(Normalised) reinforcement: k''=c<sub>r</sub>/c<sub>r,u,WWMc</sub>",
      range = c(0,1.05)
    ),
    legend = list(
      title = list(
        text = 'Model/Load sharing rule'
      )
    )
  )
  #make long data - of existing load sharing rules
  dk <- data.frame(
    model = rep(c('FBMc', 'FBMcw'), each = nrow(dp)),
    loadsharing = rep(dp$loadsharing, 2),
    betaF = rep(dp$betaF, 2),
    kk = c(dp$kk_fbmc, dp$kk_fbmcw)
  )
  #add points in loop
  loadsharingunique <- unique(dk$loadsharing)
  #make hoverinfo
  dk$HoverText <- with(dk, paste(
    'Model: ', model, '<br>',
    'Load sharing rule: ', loadsharing, '<br>',
    '\u03b2<sub>F</sub>: ', signif(betaF, nsignif), '<br>',
    "k'': ", signif(kk, nsignif),
    sep=''
  ))
  #plot
  for (i in 1:length(loadsharingunique)){
    p <- plotly::add_trace(p,
      data = dk[dk$loadsharing==loadsharingunique[i],],
      type = 'scatter',
      mode = 'markers+lines',
      x = ~betaF,
      y = ~kk,
      marker = list(
        size = 8,
        fill = ~loadsharing,
        line = list(
          color = 'black',
          width = 1
        )
      ),
      line = list(
        color = ~loadsharing,
        dash = 'dot'
      ),
      name = loadsharingunique[i],
      text = ~HoverText,
      hoverinfo = 'text',
      showlegend = T
    )
  }
  #return
  return(p)
}
