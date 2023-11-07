#' Run the Shiny App
#'
#' @export
#'
#' @importFrom shiny shinyApp
run_nirsex <- function(){
  shinyApp(app_ui, app_server)
}
