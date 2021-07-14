#' Report possible optimizations in `.R` files.
#'
#' @param files A character vector with paths to files to optimize.
#' @param optimizers A named list of optimizer functions.
#'
#' @export
#'
generate_files_opt_report <- function(files, optimizers = rco:::all_optimizers) {
  # An introductory message, as the function could take quite some time depending on the number and size of files
  message("Sit back, this may take some time...")
  # Initializations
  final_result <- codes <- list()
  # Read the contents of a file. 3 files will form a list/vector/df of size 3
  codes <- lapply(files, read_code_file)
  # Naming each entry in the list
  names(codes) <- files
  # Setting up the progress bar
  pb_outer <- utils::txtProgressBar(1, length(codes)*length(optimizers), style=3)
  pb_itr <- 0
  # Probing the contents of each file:
  for (i in seq_len(length(codes))) {
    # Original code from the file
    og_code <- codes[[i]]
    # Remove whitespaces for comparsion purposes
    og_code_no_ws <- gsub(" ", "", og_code)
    # Initialize an empty vector
    opt_used_in_i <- vector(mode = "character")
    # Running each file through each of the optimizers specified
    for(j in seq_along(optimizers)) {
      #Incrementing the progress bar
      pb_itr <- pb_itr + 1
      utils::setTxtProgressBar(pb_outer, pb_itr)
      # act_optim is the current optimizer
      act_optim <- optimizers[[j]]
      # act_optim_name is the name of the current optimizer
      act_optim_name <- names(optimizers)[[j]]
      # Optimized code
      opt_j_code <- act_optim(og_code)
      # Remove whitespaces for comparsion purposes
      opt_j_code_no_ws <- gsub(" ", "", opt_j_code[[1]])
      # If code was optimized, then append the optimizer name to the file
      if(!isTRUE(all.equal(og_code_no_ws, opt_j_code_no_ws)))
        opt_used_in_i <- append(opt_used_in_i, act_optim_name)
    }
    # Setting the name for the files in the resultant list
    code_file_name <- names(codes)[i]
    final_result[[code_file_name]] <- opt_used_in_i
  }
  # Removing all entries with `character(0)` or no optimizations
  final_result <- final_result[lapply(final_result, length) > 0]
  # If no optimization required
  if(length(final_result) == 0) {
    message("No more optimizations are required in your script(s)")
  } else { #If optimization is required
    cat("\n")
    message(sprintf("`rco` found %d files for optimization", as.integer(length(final_result))))
    message("Use the `rco::rco_gui()` function for detailed comparsion")
    message("Here are the file(s) that could be optimized along with the required optimizers:")
    cat("\n")
    print(final_result)
  }
}
