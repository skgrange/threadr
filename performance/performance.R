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




library(microbenchmark)

time_pad_benchmark <- microbenchmark(
  dplyr = time_pad(data_db, "hour", by = c("site", "site_name", "variable"),
                 do = TRUE),
  plyr = time_pad(data_db, "hour", by = c("site", "site_name", "variable"),
                 do = FALSE),
  plyr_merge = time_pad(data_db, "hour", by = c("site", "site_name", "variable"),
                        do = FALSE, merge = TRUE),
  dplyr_merge = time_pad(data_db, "hour", by = c("site", "site_name", "variable"),
                         do = TRUE, merge = TRUE),
  times = 5
)

autoplot(time_pad_benchmark)
