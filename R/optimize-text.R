#' Optimize text containing code
#'
#' Performs the desired optimization strategies in the text.
#' Carefully examine the results after running this function!
#'
#' @param text A character vector with the code to optimize.
#' @param optimizers A list of optimizer functions.
#'
#' @export
#'
optimize_text <- function(text, optimizers = all_optimizers) {
  # create a temp file to write text, and apply optimizers
  tmp_file <- tempfile()
  write_code_file(text, tmp_file)
  optimize_files(tmp_file, optimizers, overwrite = TRUE)
  res <- paste(read_code_file(tmp_file), collapse = "\n")
  cat(res)
  return(invisible(res))
}
