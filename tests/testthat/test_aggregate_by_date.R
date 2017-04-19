context("Date aggregator")

test_that("Test date aggregator", {
  
  # # Load
  # df <- readRDS(system.file("extdata/data_airbase_single_sampled.rds",
  #                           package = "threadr"))
  # 
  # # Reshape
  # df <- tidyr::gather(df, variable, value, -date, -site, -site_type, -country)
  # df <- threadr::factor_coerce(df)
  # df <- data.frame(df)
  
  # Load
  df <- readRDS(system.file("extdata/data_airbase_single_sampled_longer.rds",
                            package = "threadr"))

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