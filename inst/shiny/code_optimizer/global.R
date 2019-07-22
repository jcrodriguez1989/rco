.optimizers <- rco::all_optimizers

.random_gif <- function() {
  search_raw <- try({
    GET(
      url = "http://api.giphy.com/",
      path = "/v1/gifs/random",
      query = list(api_key = "GAUeYalixQovJJGRACGEaKRpNunOoH1q")
    )
  })

  res <- ""
  if (!inherits(search_raw, "try-error") && search_raw$status_code == 200) {
    res <- paste0(
      '<img src="',
      content(search_raw)$data$images$fixed_height$url,
      '">'
    )
  }
  res
}
