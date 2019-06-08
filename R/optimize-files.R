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
#'
#' @export
#'
optimize_files <- function(files, optimizers = all_optimizers,
                           overwrite = FALSE) {
  # read files content
  codes <- lapply(files, read_code_file)
  names(codes) <- files

  optim_codes <- codes
  # apply one by one each optimization strategy.
  # note that they are applied one after the other (in order)
  for (act_optim in optimizers) {
    optim_res <- act_optim(optim_codes)
    optim_codes <- optim_res$codes
    # todo: think! will I need anything else to get from an optimizing function?
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
