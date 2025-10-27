#' Launch the Shiny app
#'
#' Launches the Shiny application bundled in this package under `inst/app/`.
#'
#' @param ... Additional arguments passed to [shiny::shinyAppDir()].
#' @return No return value; called for side effects.
#' @export
#' @examples
#' if (interactive()) {
#'   myappkg::launch_app()
#' }
launch_app <- function(...) {
  app_dir <- system.file("app", package = "myappkg")  # looks for inst/app
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Cannot find app directory. Ensure files are under inst/app/.", call. = FALSE)
  }
  shiny::shinyAppDir(app_dir, ...)
}
