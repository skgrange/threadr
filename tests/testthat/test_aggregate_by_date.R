context("Date aggregator")

test_that("Test date aggregator", {
  
  # Load
  df <- readRDS(
    system.file(
      "extdata/data_airbase_single_sampled_longer.rds", 
      package = "threadr"
    )
  )
  
  # Default use
  expect_equal(nrow(aggregate_by_date(df)), 6572)
  
  # Monthly
  expect_equal(nrow(aggregate_by_date(df, interval = "month")), 10)
  
  # No padding, incorrect in this example
  expect_equal(nrow(aggregate_by_date(df, interval = "month", pad = FALSE)), 4)
  
  # Different summary functions
  expect_equal(
    nrow(aggregate_by_date(df, interval = "month", summary = "median")),
    10
  )
  
  expect_equal(
    nrow(aggregate_by_date(df, interval = "month", summary = "count")),
    10
  )
  
  # Single group by
  expect_equal(
    nrow(aggregate_by_date(df, interval = "month", by = "variable")),
    20
  )
  
  # Multiple group by
  expect_equal(
    nrow(aggregate_by_date(df, interval = "month", by = c("site", "variable", "country"))),
    20
  )
  
  # Threshold
  expect_equal(
    unique(aggregate_by_date(df, interval = "month", threshold = 0.75)$value),
    NA
  )
  
})


test_that("Test `mean_wd`", { 
  
  # Example vectors from openweather map forecast
  wd <- c(280.502, 267.001, 248.504, 264.001, 282.508, 274, 174, 171.002, 
          197.502, 270.503, 253.507, 262.004, 159.002, 156.503, 115.501, 
          110.001, 158.502, 248.501, 292.001, 280.501, 130.007, 223.505, 
          228.501, 273.508, 118.501, 57.0037, 80.5012, 72.5002, 81.5104, 
          35.5016, 152.503, 332.001, 90.501, 164.001, 263.505, 72.5173, 
          279.501, 305.001)
  
  ws <- c(3.05, 2.36, 2.42, 2.77, 1.83, 1.06, 1.14, 1.21, 1.21, 1.78, 
          1.86, 3.21, 1.17, 1.32, 1.16, 1.06, 1.22, 1.96, 2.12, 1.4, 0.71, 
          0.89, 1.21, 1.22, 1.56, 1.76, 2.31, 1.96, 1.56, 1.31, 1.16, 1.05, 
          1.71, 0.86, 3.41, 1.66, 4, 2.41)
  
  # Unweighted by wind speeds
  # Test with openair::timeAverage(select(data_forecast, -ws), "year")$wd
  expect_equal(mean_wd(wd, na.rm = TRUE), 218.3302, tolerance = .001)
  
  # Weighted by wind speeds, the resultant vector average wind speed
  # Test with openair::timeAverage(data_forecast, "year")$wd
  expect_equal(mean_wd(wd, ws, na.rm = TRUE), 251.4836, tolerance = .001)
  
})
