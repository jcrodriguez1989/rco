#!/usr/bin/env Rscript

library("devtools")
library("rvest", quietly = TRUE)

options(repos = list(CRAN = "http://mirror.las.iastate.edu/CRAN/"))

# install latest rco
install_github("jcrodriguez1989/rco")

cran_url <- "https://cran.r-project.org/"
pkg_name <- Sys.getenv("RCO_PKG")

if (pkg_name == "") {
  # sample a CRAN package
  # get all CRAN packages list
  url <- paste0(cran_url, "web/packages/available_packages_by_date.html")
  xpath <- "/html/body/table"
  testthat_pkgs <- Sys.getenv("RCO_W_TESTTHAT") != ""
  if (testthat_pkgs) {
    # check pkg that Suggests `testthat`
    url <- paste0(cran_url, "web/packages/testthat/index.html")
    xpath <- "/html/body/table[3]"
  }
  packages <- url %>%
    read_html() %>%
    html_nodes(xpath = xpath) %>%
    html_table()
  packages <- packages[[1]]

  # sample one package and download it
  if (!testthat_pkgs) {
    act_pkg <- packages[sample(seq_len(nrow(packages)), 1), ]
    pkg_name <- act_pkg$Package
    cat(paste0(
      "\nPackage to test: ", pkg_name, "\n",
      act_pkg$Title, "\n\n"
    ))
  } else {
    packages <- unlist(strsplit(packages[,2], ", "))
    pkg_name <- packages[sample(seq_along(packages), 1)]
    cat(paste0(
      "\nPackage to test: ", pkg_name, "\n\n"
    ))
  }
}

pkg_url <- paste0(cran_url, "package=", pkg_name)
pkg_dwn_link <- pkg_url %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/table") %>%
  html_table()
pkg_dwn_link <- pkg_dwn_link[[2]]
pkg_dwn_file <- pkg_dwn_link[grep("source:", pkg_dwn_link[, 1]), 2]
pkg_dwn_link <- paste0(cran_url, "src/contrib/", pkg_dwn_file)
# dwn_dir <- tempdir()
dwn_dir <- getwd()
lib_file <- paste0(dwn_dir, "/", pkg_dwn_file)
download.file(pkg_dwn_link, lib_file)

# extract package and copy it to a package'_opt' folder
untar(lib_file, exdir = dwn_dir)
pkg_dir <- paste0(dwn_dir, "/", pkg_name)
pkg_dir_opt <- paste0(pkg_dir, "_opt")
dir.create(pkg_dir_opt)
invisible(
  file.copy(dir(pkg_dir, full.names = TRUE), pkg_dir_opt, recursive = TRUE)
)

# optimize package'_opt'
library("withr")
library("rco")

sessionInfo()

with_dir(pkg_dir_opt, {
  act_files <- dir("R")
  opt_files <- optimize_files(paste0("R/", act_files), overwrite = TRUE)
  cat(paste0(
    "\nOptimized ", sum(opt_files), " of ", length(act_files),
    " files:\n"
  ))
  cat(paste0(paste(act_files[opt_files], collapse = "\n"), "\n"))
})

# We finnished if the package does not have test cases or was not optimized
if (!(file.exists(paste0(pkg_dir, "/tests/testthat.R")) &&
  file.exists(paste0(pkg_dir, "/tests/testthat/")))) {
  cat("Package does not contain testthat suite.\n")
  quit(save = "no")
}

if (sum(opt_files) == 0) {
  cat("Package not optimized.\n")
  quit(save = "no")
}

# install package from CRAN (to solve dependencies)
install.packages(pkg_name)

# test and time both packages
with_dir(pkg_dir, {
  build(vignettes = FALSE, quiet = TRUE)
  test_time <- system.time({
    test_res <- test()
  })
})

with_dir(pkg_dir_opt, {
  build(vignettes = FALSE, quiet = TRUE)
  test_time_opt <- system.time({
    test_res_opt <- test()
  })
})

test_res <- lapply(test_res, function(act_res)
  sapply(act_res$results, function(act_res_res)
    is(act_res_res, "expectation_success")))

test_res_opt <- lapply(test_res_opt, function(act_res)
  sapply(act_res$results, function(act_res_res)
    is(act_res_res, "expectation_success")))

if (!all.equal(test_res, test_res_opt)) {
  error("Differing test results.\n")
  quit(save = "no")
}

test_speedup <- test_time / test_time_opt
cat("\n\n*****************************\n")
cat("rco obtained a speedup of:\n")
print(test_speedup)
