#' Optimize `.R` files
#'
#' Performs the desired optimization strategies in the files specified.
#' Carefully examine the results after running this function!
#' If several files interact between them (functions from one file use
#' functions from the other), then optimizing all of them together gives more
#' information to rco.
#'
#' @param files A character vector with paths to files to optimize.
#' @param optimizers A list of optimizer functions.
#' @param overwrite A logical indicating if files should be overwritten, or
#'   saved into new files with "optimized_" prefix.
#' @param iterations Numeric indicating the number of times optimizers should
#'   pass. If there was no change after current pass, it will stop.
#'
#' @export
#'
optimize_files <- function(files, optimizers = all_optimizers,
                           overwrite = FALSE, iterations = Inf) {
  # read files content
  codes <- lapply(files, read_code_file)
  names(codes) <- files

  n_iter <- 0
  optim_codes <- codes
  act_codes <- NA
  while (n_iter < iterations && !isTRUE(all.equal(act_codes, optim_codes))) {
    act_codes <- optim_codes
    # apply one by one each optimization strategy.
    # note that they are applied one after the other (in order)
    for (act_optim_name in names(optimizers)) {
      act_optim <- optimizers[[act_optim_name]]
      optim_res <- try({
        act_optim(optim_codes)
      }, silent = TRUE)
      if (inherits(optim_res, "try-error")) {
        warning(paste("Optimizer", act_optim_name, "had errors:\n", optim_res))
        optim_res <- list(codes = optim_codes)
      }
      optim_codes <- optim_res$codes
    }
    n_iter <- n_iter + 1
    cat(paste0("Optimization number ", n_iter, "\n"))
  }

  # check which codes had been optimized
  changed <- sapply(seq_along(codes), function(i) {
    !isTRUE(all.equal(codes[[i]], optim_codes[[i]]))
  })

  # save optimized files
  changed_codes <- optim_codes[changed]
  if (!overwrite && length(changed_codes) > 0) {
    names(changed_codes) <- paste0(
      dirname(names(changed_codes)),
      "/optimized_", basename(names(changed_codes))
    )
  }
  invisible(lapply(names(changed_codes), function(act_file) {
    write_code_file(changed_codes[[act_file]], act_file)
  }))

  return(invisible(changed))
}
