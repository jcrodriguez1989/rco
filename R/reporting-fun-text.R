#' Report possible optimizations in `.R` code snippet.
#'
#' @param text A character vector with the code to optimize.
#' @param optimizers A named list of optimizer functions.
#'
#' @export
#'
generate_text_opt_report <- function(text, optimizers = all_optimizers) {
  input_code_snippet <- tempfile()
  rco:::write_code_file(text, input_code_snippet)
  message("Ignore the name that will be assigned to your text/code snippet")
  cat("\n")
  generate_files_opt_report(input_code_snippet, optimizers)
}
