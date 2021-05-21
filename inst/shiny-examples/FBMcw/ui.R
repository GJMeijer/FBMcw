#v0.1 - 20210416 - first working version

################
### DEFINE UI ###
#################

#make UI
ui <- shiny::navbarPage(
  title = "FBMc/FBMcw",
  position = "fixed-top",
  collapsible = TRUE,

  shiny::tabPanel(
    "Analysis",
    tags$style(type="text/css", "body {padding-top: 70px;}"),

    #FIRST ROW - MOBILISATION
    shiny::fluidRow(
      h3('Plot 1: Mobilisation of reinforcement in FBMc/FBMcw', align="center")
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::sliderInput(
            "dr",
            label = HTML(paste0("Root diameter range (d", tags$sub("r,min"), " - d", tags$sub("r,max"), ') [mm]', sep='')),
            min = 0.1,
            value = c(1,5),
            max = 10,
            step = 0.1
          ),
          shiny::sliderInput(
            "betaphi",
            label = HTML(paste0("Root area ratio distribution power coefficient (\u03b2", tags$sub("\u03c6"), ")", sep='')),
            min = -2,
            value = -0.1, #-1
            max = 2,
            step = 0.1
          ),
          shiny::sliderInput(
            "betat",
            label = HTML(paste0("Tensile strength power coefficient (\u03b2", tags$sub("t"), ")", sep='')),
            min = -2,
            value = -0.2,
            max = 2,
            step = 0.1
          ),
          shiny::sliderInput(
            "betaF",
            label = HTML(paste0("Load sharing parameter (\u03b2", tags$sub("F"), ")", sep='')),
            min = -2,
            value = 1.1,  #1.8
            max = 3,
            step = 0.1
          ),
          shiny::sliderInput(
            "kappa",
            label = "Weibull survival shape parameter (\u03BA)",
            min = 0.1,
            value = 5.0,
            max = 10,
            step = 0.1
          ),
        )
      ),
      shiny::column(8,
        plotly::plotlyOutput("p_traces")
      )
    ),

    #SECOND ROW: LOAD SHARING PREDICTIONS
    shiny::fluidRow(
      h3('Plot 2: Comparison of load sharing rules', align="center")
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::sliderInput(
            "betaE",
            label = HTML(paste0("Root stiffess power coefficient (\u03b2", tags$sub("E"), ")", sep='')),
            min = -2,
            value = -0.1,
            max = 2,
            step = 0.1
          ),
          shiny::sliderInput(
            "betaL",
            label = HTML(paste0("Root length power coefficient (\u03b2", tags$sub("L"), ")", sep='')),
            min = -2,
            value = 0.6,
            max = 2,
            step = 0.1
          )
        )
      ),
      shiny::column(8,
        plotly::plotlyOutput("p_loadsharing")
      )
    ),

    #THIRD ROW: ABSOLUTE VALUES
    shiny::fluidRow(
      h3("Plot 3: Peak root reinforcement predictions ('root cohesion')", align="center")
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::sliderInput(
            "phirt",
            label = HTML(paste0("(Total) root area ratio (\u03c6", tags$sub("r,t"), ") [%]", sep='')),
            min = 0.0,
            value = c(0.5),
            max = 2.5,
            step = 0.01
          ),
          shiny::sliderInput(
            "tru0",
            label = HTML(paste0("Root tensile strength in root with 1 mm diameter (t", tags$sub("r,u,0"), ") [MPa]", sep='')),
            min = 0,
            value = 10,
            max = 100,
            step = 0.1
          ),
          shiny::sliderInput(
            "k",
            label = "WWM root orientation factor k'",
            min = 0,
            value = 1.2,
            max = 2,
            step = 0.01
          )
        )
      ),
      shiny::column(8,
        plotly::plotlyOutput("p_cohesion")
      )
    ),

    #FOURTH ROW: DISCRETISATION EFFECT
    shiny::fluidRow(
      h3("Plot 4: Effect of using discrete diameter classes (FBM and FBMw models)", align="center")
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::wellPanel(
          shiny::sliderInput(
            "nc",
            label = HTML(paste0("Number of discrete diameter classes (n", tags$sub("c"), ")", sep='')),
            min = 1,
            value = 3,
            max = 25,
            step = 1
          )
        )
      ),
      shiny::column(8,
        plotly::plotlyOutput("p_classtraces")
      )
    )
  ),

  #TAB: DOCUMENTATION
  shiny::tabPanel(
    "Documentation",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    shiny::withMathJax(),
    shiny::includeMarkdown('www/FBMc_documentation.rmd')
  )
)
