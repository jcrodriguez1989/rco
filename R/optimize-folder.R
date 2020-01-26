#' Optimize a folder with `.R` files.
#'
#' Performs the desired optimization strategies in all the `.R` in a directory.
#' Carefully examine the results after running this function!
#' If several files interact between them (functions from one file use
#' functions from the other), then optimizing all of them together gives more
#' information to rco.
#'
#' @param folder Path to a directory with files to optimize.
#' @param optimizers A named list of optimizer functions.
#' @param overwrite A logical indicating if files should be overwritten, or
#'   saved into new files with "optimized_" prefix.
#' @param iterations Numeric indicating the number of times optimizers should
#'   pass. If there was no change after current pass, it will stop.
#' @param pattern An optional regular expression. Only file names which match
#'   the regular expression will be optimized.
#' @param recursive A logical value indicating whether or not files
#'   in subdirectories of `folder` should be optimized as well.
#'
#' @export
#'
optimize_folder <- function(folder, optimizers = all_optimizers,
                            overwrite = FALSE, iterations = Inf,
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
  message("Files to optimize:")
  message(paste(to_opt_files, collapse = "\n"))
  optimize_files(
    files = to_opt_files, optimizers = optimizers, overwrite = overwrite,
    iterations = iterations
  )
}
