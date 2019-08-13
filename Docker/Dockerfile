# Use latest R
FROM r-base:latest

# Install required R packages, and their OS dependencies
ENV DEBIAN_FRONTEND=noninteractive

RUN apt update
RUN apt --assume-yes install libcurl4-openssl-dev libgit2-dev libssl-dev libxml2-dev

RUN R -e "\
  options(repos = \
    list(CRAN = 'http://mirror.las.iastate.edu/CRAN/')); \
  install.packages('devtools'); \
  install.packages('dplyr'); \
  install.packages('microbenchmark'); \
  install.packages('rvest'); \
  install.packages('testthat'); \
  install.packages('withr');"

# Copy the script
RUN mkdir /rco_files
WORKDIR /rco_files
COPY optimize_rand_pkg.R ./

# Run the script and save results in /rco_results
RUN mkdir /rco_results
CMD Rscript optimize_rand_pkg.R && mv /rco_files/* /rco_results/
