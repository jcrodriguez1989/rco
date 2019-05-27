# Parses text to a flat parsed data data.frame
#
# @param text Text to parse.
#
parse_flat_data <- function(text) {
  parsed_text <- base::parse(text = text)
  utils::getParseData(parsed_text)
}

# Deparses a flat parsed data data.frame to text
#
# @param fpd a flat parsed data data.frame to deparse.
#
deparse_flat_data <- function(fpd) {
  fpd_terms <- fpd[fpd$terminal, ]

  old_line <- 1
  old_col <- 0
  res <- ""
  for (i in seq_len(nrow(fpd_terms))) {
    act_pd <- fpd_terms[i, ]
    act_line <- act_pd$line1
    new_lines <- act_line - old_line
    if (new_lines > 0) {
      old_col <- 0
      res <- paste0(res, paste0(rep("\n", new_lines), collapse = ""))
    }
    act_col <- act_pd$col1
    res <- paste0(res, paste0(rep(" ", max(0, act_col - old_col - 1)), collapse = ""))
    res <- paste0(res, act_pd$text)
    old_line <- act_line
    old_col <- act_pd$col2
  }
  return(res)
}

