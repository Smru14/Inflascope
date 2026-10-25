#' Launch the inflation intelligence Shiny dashboard
#' @param ... Additional arguments to shiny::runApp().
#' @return Starts an interactive application.
#' @examples
#' if (interactive()) launch_app()
#' @export
launch_app <- function(...) {
  path <- system.file("shiny", "app.R", package="inflascope")
  if (!nzchar(path)) stop("Shiny application missing")
  shiny::runApp(path, ...)
}
