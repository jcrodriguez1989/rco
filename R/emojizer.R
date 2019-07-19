#' Optimizer: Emojizer
#'
#' Performs one emojizer pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "train <- function(param) {",
#'   "  param + 100",
#'   "}",
#'   sep = "\n"
#' )
#' cat(emojizer(list(code))$codes[[1]])
#' @export
#'
emojizer <- function(texts) {
  res <- list(codes = texts)
  if (require("emo")) {
    res$codes <- lapply(texts, emojize_text)
  }
  return(res)
}

# Performs a emojizer pass on one text.
#
# @param text A character vector with code to optimize.
#
emojize_text <- function(text) {
  fpd <- parse_flat_data(text)
  fpd <- flatten_leaves(fpd)
  if (nrow(fpd) > 0) {
    fpd <- emojize_fpd(fpd)
  }
  deparse_flat_data(fpd)
}

# Performs the emojizer pass.
#
# @param fpd A flat parse data data.frame with code to optimize.
#
emojize_fpd <- function(fpd) {
  emojizable_ids <- get_emojizable_ids(fpd)
  mapping <- get_emoji_mapping(unique(fpd$text[fpd$id %in% emojizable_ids]))
  fpd$text[fpd$id %in% emojizable_ids] <-
    mapping[fpd$text[fpd$id %in% emojizable_ids]]
  fpd
}

get_emojizable_ids <- function(fpd) {
  emojizable_ids <- fpd$id[fpd$token %in% c("SYMBOL", "SYMBOL_FORMALS")]

  # get names of defined functions
  fun_def_prnts <- fpd$parent[fpd$token == "FUNCTION"]
  def_funs <- sapply(fun_def_prnts, function(act_fun_def) {
    fun_def_prnt_id <- fpd$parent[fpd$id == fun_def_prnts]
    res <- NA
    prnt_sblngs <- fpd[fpd$parent == fun_def_prnt_id, ]
    if (any(assigns %in% prnt_sblngs$token)) {
      res <- prnt_sblngs$text[prnt_sblngs$token == "SYMBOL"]
    }
    res
  })

  # get calls of defined functions
  def_fun_calls_prnts <- fpd$parent[
    fpd$token == "SYMBOL_FUNCTION_CALL" & fpd$text %in% def_funs]
  fun_calls_emoj_ids <- fpd$id[
    fpd$parent %in% def_fun_calls_prnts &
      fpd$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL_SUB")]
  c(emojizable_ids, fun_calls_emoj_ids)
}

get_emoji_mapping <- function(names) {
  jis <- emo::jis

  ji_map <- do.call(rbind, lapply(seq_len(nrow(jis)), function(i) {
    names <- jis[i, "name", drop = TRUE]
    names <- c(names, jis[i, "keywords", drop = TRUE][[1]])
    names <- c(names, jis[i, "aliases", drop = TRUE][[1]])
    cbind(names, jis[i, "emoji"])
  }))

  # remove emojis with name longer than `names`
  name_max_len <- max(nchar(names))
  colnames(ji_map) <- c("name", "emoji")
  ji_map <- ji_map[nchar(ji_map[, "name"]) > 0, ]
  ji_map <- ji_map[nchar(ji_map[, "name"]) <= name_max_len, ]
  # random sort
  ji_map <- ji_map[sample(seq_len(nrow(ji_map))), ]
  # try to match longer words first
  ji_map <- ji_map[order(nchar(ji_map[, "name"]), decreasing = TRUE), ]

  # remove emojis that are not substrings of names
  ji_map <- ji_map[
    sapply(ji_map[, "name"], function(act_ji) any(grepl(act_ji, names))), ]

  new_names <- names
  for (i in seq_len(nrow(ji_map))) {
    new_names <- sub(ji_map[i, "name"], ji_map[i, "emoji"], new_names)
  }

  new_names[new_names != names] <- paste0("`", new_names[new_names != names], "`")
  names(new_names) <- names
  new_names
}
