#v0.1 - 20210416 - first working version

#####################
### DEFINE SERVER ###
#####################

# Define server
server <- function(input, output) {

  #create dataframe for strain
  epsr0rel <- reactive({
    generate_strainrange(input$dr[1], input$dr[2], input$betaF, input$betat, margin = 1.5)
  })
  #add FBMc and FBMcw predictions to dataframe with strains
  deps <- reactive({
    data.frame(
      epsr0rel = epsr0rel(),
      kk_fbmc = calc_kk_fbmc(epsr0rel(), input$dr[1], input$dr[2], input$betaF, input$betat, input$betaphi),
      kk_fbmcw = calc_kk_fbmcw(epsr0rel(), input$dr[1], input$dr[2], input$betaF, input$betat, input$betaphi, input$kappa)
    )
  })
  #create strain plot
  output$p_traces <- renderPlotly({
    plotly_app_kkfbmc_kkfbmcw(deps())
  })

  #create all load sharing rules
  dm2 <- reactive({
    generate_loadsharingrules(
      betat = input$betat,
      betaE = input$betaE,
      betaL = input$betaL,
      betaF = input$betaF
    )
  })
  #create FBMc and FBMcw predictions for all load sharing assumptions
  dm <- reactive(
    cbind(
      dm2(),
      kk_fbmc = calc_kku_fbmc(input$dr[1], input$dr[2], dm2()$betaF, input$betat, input$betaphi),
      kk_fbmcw = calc_kku_fbmcw(input$dr[1], input$dr[2], dm2()$betaF, input$betat, input$betaphi, input$kappa)
    )
  )
  #create range of betaF values
  dbetaF <- reactive({
    generate_loadsharingrange(betaF = dm2()$betaF)
  })
  #create FBMc and FBMcw predictions for all betaF values
  dF <- reactive({
    data.frame(
      betaF = dbetaF(),
      kk_fbmc = calc_kku_fbmc(input$dr[1], input$dr[2], dbetaF(), input$betat, input$betaphi),
      kk_fbmcw = calc_kku_fbmcw( input$dr[1], input$dr[2], dbetaF(), input$betat, input$betaphi, kappa = input$kappa)
    )
  })
  #create betaF plot
  output$p_loadsharing <- renderPlotly({
    plotly_app_loadsharingtraces(dF(), dm())
  })

  #root area ratio distribution parameter phir0
  phir0 <- reactive({
    calc_phir0(
      input$phirt / 100,  #percentage to fraction
      input$dr[1],
      input$dr[2],
      input$betaphi
    )
  })
  #claculate WWMc solution
  cru_wwmc <- reactive({
    calc_cru_wwmc(
      input$dr[1],
      input$dr[2],
      input$tru0 * 1000,   #MPa to kPa
      input$betat,
      phir0(),
      input$betaphi,
      k = input$k
    )
  })
  #plot cohesion predictions
  output$p_cohesion <- renderPlotly({
    plotly_app_cohesionpredictions(dm(), cru_wwmc())
  })

  #create plot
  output$p_classtraces <- renderPlotly({
    plotly_stackedtrace_cr_fbm_fbmw(
      input$dr[1],
      input$dr[2],
      input$betaF,
      input$tru0 * 1000,
      input$betat,
      input$phirt / 100,
      input$betaphi,
      input$kappa,
      input$nc,
      input$k
    )
  })
}
