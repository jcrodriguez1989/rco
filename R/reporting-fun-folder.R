#' Report possible optimizations in folder containing `.R` files.
#'
#' @param folder Path to a directory with files to optimize.
#' @param optimizers A named list of optimizer functions.
#' @param pattern An optional regular expression. Only file names which match
#'   the regular expression will be optimized.
#' @param recursive A logical value indicating whether or not files
#'   in subdirectories of `folder` should be optimized as well.
#'   
#' @export
#' 
generate_folder_opt_report <- function(folder, optimizers = all_optimizers, 
                                       pattern = "\\.R$", recursive = TRUE) {
  if (!dir.exists(folder)) {
    stop("The specified folder does not exist.")
  }
  to_opt_files <- list.files(
    path = folder, pattern = pattern, full.names = TRUE, recursive = recursive
  )
  if (length(to_opt_files) == 0) {
    return()
  }
  message("Files to check for optimization:")
  message(paste(to_opt_files, collapse = "\n"))
  generate_files_opt_report(files = to_opt_files, 
                            optimizers = optimizers)
}