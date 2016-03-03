context("Time padder")

test_that("Test time padder with single site", {
  
  df <- readRDS("../../data/data_airbase_single_sampled.rds")
  
  # Simple usage
  expect_equal(nrow(time_pad(df, interval = "hour")), 6572)
  expect_equal(nrow(time_pad(df, interval = "min")), 394261)
  
  expect_equal(nrow(time_pad(df, interval = "hour", round = "day")), 6600)
  
  expect_equal(nrow(time_pad(df, interval = "hour", round = "year")), 8784)
  
  # Group by
  expect_equal(nrow(time_pad(df, interval = "hour", by = "site")), 6572)
  # Multiple
  expect_equal(nrow(time_pad(
    df, interval = "hour", by = c("site", "country", "site_type"))), 6572)
  
})


test_that("Test time padder with multiple sites", {
  
  df <- readRDS("../../data/data_airbase_sampled.rds")
  
  expect_equal(nrow(time_pad(df, interval = "hour", by = "site")), 14226)
  
  expect_equal(nrow(
    time_pad(df, interval = "hour", by = c("site", "country", "site_type"))), 
    14226)
  
})


# benchmark <- microbenchmark(
#   
#   do = time_pad(data_uk_sampled, by = c("site", "country", "site_type"),
#                 interval = "min"),
#   
#   do_merge = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                       merge = TRUE, interval = "min"),
#   
#   plyr = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                   do = FALSE, interval = "min"),
#   
#   plyr_merge = time_pad(data_uk_sampled, by = c("site", "country", "site_type"), 
#                         merge = TRUE, do = FALSE, interval = "min"),
#   
#   times = 10
# )
# 
# autoplot(benchmark)