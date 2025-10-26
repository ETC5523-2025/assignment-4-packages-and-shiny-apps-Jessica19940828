#' Launch the ausbushfire Shiny App
#'
#' This function launches the Shiny application included in the
#' \code{ausbushfire} package.
#'
#' @export
#' @importFrom shiny runApp
#'
#' @examples
#' \dontrun{
#'   # This will launch the app
#'   run_app()
#' }
run_app <- function() {
  # Find the 'app' directory within the installed package
  app_dir <- system.file("app", package = "ausbushfire")

  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing 'ausbushfire'.",
         call. = FALSE)
  }

  shiny::runApp(app_dir, launch.browser = TRUE)
}
