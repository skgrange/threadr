context("Time padder")

test_that("Test time padder with single site", {
  
  df <- readRDS(system.file("extdata/data_airbase_single_sampled.rds", 
                            package = "threadr"))
  
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
  
  df <- readRDS(system.file("extdata/data_airbase_sampled.rds", 
                            package = "threadr"))
  
  expect_equal(nrow(time_pad(df, interval = "hour", by = "site")), 14226)
  
  expect_equal(nrow(
    time_pad(df, interval = "hour", by = c("site", "country", "site_type"))), 
    14226)
  
})
