download_pkg <- function(pkg_name) {
  down_dir <- tempdir()
  pkg_dir <- ""
  pkg_name <- trimws(pkg_name)

  if (grepl("/", pkg_name)) {
    pkg_dir <- download_gh_pkg(pkg_name, down_dir)
  } else {
    pkg_dir <- download_cran_pkg(pkg_name, down_dir)
  }

  pkg_dir
}

download_gh_pkg <- function(pkg_name, down_dir) {
  # github package
  url <- paste0("https://github.com/", pkg_name, "/archive/master.zip")
  down_file <- paste0(down_dir, "/", gsub(".*/", "", pkg_name), ".zip")
  err <- try({
    download.file(url, down_file)
  }, silent = TRUE)

  if (inherits(err, "try-error")) {
    warning("Could not download package.")
    return(err)
  }

  # unzip file
  pkg_dir <- paste0(down_dir, "/", gsub(".*/", "", pkg_name), "-master")
  unlink(pkg_dir, recursive = TRUE, force = TRUE)
  err <- try({
    unzip(down_file, exdir = down_dir)
  }, silent = TRUE)

  if (inherits(err, "try-error") | length(dir(paste0(pkg_dir, "/R"))) == 0) {
    warning("Could not extract package.")
    return(err)
  }

  pkg_dir
}

download_cran_pkg <- function(pkg_name, down_dir) {
  pkg_url <- paste0("https://cran.r-project.org/package=", pkg_name)
  err <- try({
    url <- rvest::html_table(rvest::html_nodes(
      xml2::read_html(pkg_url),
      xpath = "/html/body/table"
    ))
    url <- url[[2]]
    dwn_file <- url[grep("source:", url[, 1]), 2]
    url <- paste0("https://cran.r-project.org/src/contrib/", dwn_file)
    down_file <- paste0(down_dir, "/", pkg_name, ".tar.gz")
    download.file(url, down_file)
  }, silent = TRUE)

  if (inherits(err, "try-error")) {
    warning("Could not download package.")
    return(err)
  }

  # unzip file
  pkg_dir <- paste0(down_dir, "/", pkg_name)
  unlink(pkg_dir, recursive = TRUE, force = TRUE)
  err <- try({
    untar(down_file, exdir = down_dir)
  }, silent = TRUE)

  if (inherits(err, "try-error") | length(dir(paste0(pkg_dir, "/R"))) == 0) {
    warning("Could not extract package.")
    return(err)
  }

  pkg_dir
}
