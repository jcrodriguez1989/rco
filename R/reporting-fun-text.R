#' Report possible optimizations in `.R` code snippet.
#'
#' @param text A character vector with the code to optimize.
#' @param optimizers A named list of optimizer functions.
#'
#' @export
#'
generate_text_opt_report <- function(text, optimizers = all_optimizers) {
  #Convert the given text into a temporary file
  input_code_snippet <- tempfile()
  write_code_file(text, input_code_snippet)
  message("Ignore the name that will be assigned to your text/code snippet")
  cat("\n")
  #Run the temporary file in the `generate_files_opt_report` function
  generate_files_opt_report(input_code_snippet, optimizers)
}
