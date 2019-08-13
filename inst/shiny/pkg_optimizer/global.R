.random_gif <- function() {
  search_raw <- try({
    httr::GET(
      url = "http://api.giphy.com/",
      path = "/v1/gifs/trending",
      query = list(api_key = "GAUeYalixQovJJGRACGEaKRpNunOoH1q")
    )
  })

  res <- ""
  if (!inherits(search_raw, "try-error") && search_raw$status_code == 200) {
    img <- sample(httr::content(search_raw)$data, 1)[[1]]
    res <- paste0(
      '<center><img src="',
      img$images$fixed_height$url,
      '"></center>'
    )
  }
  res
}
