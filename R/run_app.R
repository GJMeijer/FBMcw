#' Run a Shiny app that is part of this package
#'
#' @description
#' This helper function launches a Shiny app that is part of this
#' R package
#'
#' @param app_name name of the application
#' @param package_name name of the current package
#' @return numeric array with peak reinforcement preductions `cru_wwmc`
#' @examples
#' \dontrun{run_app('FBMcw')}
#' @export

run_app <- function(app_name, package_name = 'FBMcw') {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = package_name))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(app_name) || !nzchar(app_name) ||
      !app_name %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", app_name, package = package_name)
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
