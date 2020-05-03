#' rco GUI selector.
#'
#' Starts the selected rco Graphical User Interface (GUI).
#'
#' @param option A character indicating which GUI to open.
#'   One from:
#'   \itemize{
#'     \item "code_optimizer" for single code optimizing.
#'     \item "pkg_optimizer" for package optimizing.
#'   }
#'
#' @examples
#' ## Start the GUI
#' \dontrun{
#' rco_gui("code_optimizer")
#' }
#' @export
#'
rco_gui <- function(option) {
  guis <- c('"code_optimizer"', '"pkg_optimizer"')
  app_dir <- system.file("shiny", option, package = "rco")

  if (app_dir == "") {
    stop(paste("Selected GUI not found. Select one of:",
      paste(guis, collapse = "\n"),
      sep = "\n"
    ),
    call. = FALSE
    )
  }

  .depends <- NULL
  source(paste0(app_dir, "/imports.R"), local = TRUE)

  if (requireNamespace("shiny") && can_load(.depends)) {
    shiny::runApp(app_dir)
  }

  NULL
}

# Tries to load the packages
#
# @param required_pkgs A character vector with names of packages to load.
#
can_load <- function(required_pkgs) {
  non_loaded_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace)]
  res <- length(non_loaded_pkgs) == 0
  if (!res) {
    stop(paste("Selected GUI requires additional packages to be installed.",
      "Please install packages:",
      paste(non_loaded_pkgs, collapse = "\n"),
      sep = "\n"
    ),
    call. = FALSE
    )
  }
  res
}
