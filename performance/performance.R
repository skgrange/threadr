# Set-up ---------------------------
# Load packages
library(threadr)
library(ggplot2)
library(dplyr)
library(microbenchmark)

# Set global options
options(stringsAsFactors = FALSE)

# Set working directory
setwd("~/Dropbox/R/package_development/threadr/performance")


url <- "ftp://speedtest.tele2.net"
file_list <- list_files_ftp(url)
file_list <- grep("512KB|5MB|1MB|2MB", file_list, value = TRUE)

# Benchmark
benchmark <- microbenchmark(
  curl = download_ftp_file(file_list),
  base = download_ftp_file(file_list),
  times = 10
)

# Print
benchmark

# Plot
autoplot(benchmark)

# Save
ggsave("benchmark_download_ftp_file_methods.pdf")


url <- "sftp://test.rebex.net"
credentials <- "demo:password"
# list_files_ftp(url, credentials)

RCurl::curlVersion()
# install.packages("~/Desktop/RCurl", repos=NULL, type="source")



