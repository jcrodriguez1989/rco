# Addin function: Optimize active file.
#
# Apply `all_optimizers` to current open file.
#
optimize_active_file <- function() {
  # try to load required package
  req_pkgs <- c("rstudioapi")
  if (!can_load(req_pkgs)) {
    return()
  }

  # get context, get the code, optimize, and put the new code
  doc_context <- rstudioapi::getActiveDocumentContext()
  out <- optimize_text(doc_context$contents)
  rstudioapi::modifyRange(
    c(1, 1, length(doc_context$contents) + 1, 1),
    paste0(out, collapse = "\n"),
    id = doc_context$id
  )

  rstudioapi::setCursorPosition(doc_context$selection[[1]]$range)
}

# Addin function: Optimize selection.
#
# Apply `all_optimizers` to current code selection.
#
optimize_selection <- function() {
  # try to load required package
  req_pkgs <- c("rstudioapi")
  if (!can_load(req_pkgs)) {
    return()
  }

  # get context, get the code, optimize, and put the new code
  doc_context <- rstudioapi::getActiveDocumentContext()
  sel_code <- doc_context$selection[[1]]$text
  if (all(nchar(sel_code) == 0)) {
    stop("No code selected", call. = FALSE)
  }

  out <- optimize_text(sel_code)
  rstudioapi::modifyRange(
    doc_context$selection[[1]]$range,
    paste0(out, collapse = "\n"),
    id = doc_context$id
  )
}

# Tries to load the packages.
#
# @param required_pkgs A character vector with names of packages to load.
#
can_load <- function(required_pkgs) {
  non_loaded_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace)]
  res <- length(non_loaded_pkgs) == 0
  if (!res) {
    stop(paste("Selected Addin requires additional packages to be installed.",
      "Please install packages:",
      paste(non_loaded_pkgs, collapse = "\n"),
      sep = "\n"
    ),
    call. = FALSE
    )
  }
  res
}
