context("Date things")

test_that("round_date_interval at different intervals", {
  
  # An example date
  x <- lubridate::ymd_hms("2015-06-03 16:06:08")
  
  expect_identical(round_date_interval(x, "min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(plyr::round_any(x, 60), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "2 min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(plyr::round_any(x, 120), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "5 min"), 
                   lubridate::ymd_hms("2015-06-03 16:05:00"))
  
  expect_identical(plyr::round_any(x, 60 * 5), 
                   lubridate::ymd_hms("2015-06-03 16:05:00"))
  
  expect_identical(round_date_interval(x, "10 min"), 
                   lubridate::ymd_hms("2015-06-03 16:10:00"))
  
  expect_identical(plyr::round_any(x, 60 * 10), 
                   lubridate::ymd_hms("2015-06-03 16:10:00"))
  
  expect_identical(round_date_interval(x, "15 min"), 
                   lubridate::ymd_hms("2015-06-03 16:00:00"))
  
  expect_identical(round_date_interval(x, "20 min"), 
                   lubridate::ymd_hms("2015-06-03 16:00:00"))
  
  expect_identical(round_date_interval(x, "half hour"), 
                   lubridate::ymd_hms("2015-06-03 16:00:00"))
  
  expect_identical(round_date_interval(x, "hour"), 
                   lubridate::ymd_hms("2015-06-03 16:00:00"))
  
  # Centre of date interval
  expect_identical(round_date_interval(
    lubridate::ymd_hms("2015-06-03 16:02:30"), "5 min"), 
    lubridate::ymd_hms("2015-06-03 16:00:00"))
  
  # Behaviour changed, 2016-04-16
  expect_identical(round_date_interval(
    lubridate::ymd_hms("2015-06-03 16:02:30"), "5 min"), 
    lubridate::ymd_hms("2015-06-03 16:00:00"))
  
})


context("Test the low-level utility functions")

test_that("Test hexidecimal to integer function", {
  
  x <- "fff4d"
  # As binary: threadr::as.binary(strtoi(x, 16))
  expect_equal(strtoi(x, 16), 1048397)
  
})


test_that("Test Two's Complement function", {
  
  expect_identical(twos_complement(1048397), -179)
  
})


test_that("Test unnest function", {
  
  # Load data
  df <- read.csv(system.file("extdata/french_verbs.csv", package = "threadr"),
                 skip = 1, stringsAsFactors = FALSE)
  
  # Unnest variable
  df_tidy <- unnest(df, "subject", pattern = "/")
  
  # Test
  expect_equal(nrow(df_tidy), 9)
  
})
