# Reads code from file path
#
# @param file A character vector containing the code file path.
#
read_code_file <- function(file) {
  base::readLines(file, encoding = "UTF-8", warn = FALSE)
}

# Writes code to a con (normally a file path)
#
# @param code A character vector containing the code to write.
# @param con A [base::connection] object or a character string.
#
write_code_file <- function(code, con) {
  encoded_code <- base::enc2utf8(code)
  base::writeLines(encoded_code, con, useBytes = TRUE)
}
