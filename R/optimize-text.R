#' Optimize text containing code.
#'
#' Performs the desired optimization strategies in the text.
#' Carefully examine the results after running this function!
#'
#' @param text A character vector with the code to optimize.
#' @param optimizers A named list of optimizer functions.
#' @param iterations Numeric indicating the number of times optimizers should
#'   pass. If there was no change after current pass, it will stop.
#'
#' @export
#'
optimize_text <- function(text, optimizers = all_optimizers, iterations = Inf) {
  # create a temp file to write text, and apply optimizers
  tmp_file <- tempfile()
  write_code_file(text, tmp_file)
  optimize_files(tmp_file, optimizers,
    overwrite = TRUE,
    iterations = iterations
  )
  res <- paste(read_code_file(tmp_file), collapse = "\n")

  invisible(res)
}
