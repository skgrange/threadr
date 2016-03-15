context("Date things")

test_that("round_date_interval at different intervals", {
  
  # An example date
  x <- lubridate::ymd_hms("2015-06-03 16:06:08")
  
  expect_identical(round_date_interval(x, "min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_any(x, 60), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "2 min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_any(x, 120), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "5 min"), 
                   lubridate::ymd_hms("2015-06-03 16:05:00"))
  
  expect_identical(round_any(x, 60 * 5), 
                   lubridate::ymd_hms("2015-06-03 16:05:00"))
  
  expect_identical(round_date_interval(x, "10 min"), 
                   lubridate::ymd_hms("2015-06-03 16:10:00"))
  
  expect_identical(round_any(x, 60 * 10), 
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
  
  expect_less_than(round_date_interval(
    lubridate::ymd_hms("2015-06-03 16:02:30"), "5 min"), 
    lubridate::ymd_hms("2015-06-03 16:05:00"))
  
})


test_that("Unix time correct-ness", {
  
  # Get an example of unix time
  # as.numeric(Sys.time())
  date_unix <- 1437896406
  # This is 2015-07-26 07:40:06 UTC
  
  # Parse the unix time into dates with tz info
  date_london <- as.POSIXct(date_unix, origin = "1970-01-01", tz = "Europe/London")
  date_utc <- as.POSIXct(date_unix, origin = "1970-01-01", tz = "UTC")
  date_nz <- as.POSIXct(date_unix, origin = "1970-01-01", tz = "Pacific/Auckland")
  
  # How I parse dates
  date_normal <- lubridate::ymd_hms("2015-07-26 07:40:06")
  
  # Only testing the base as.numeric function
  expect_equal(unix_time(date_london), date_unix)
  expect_equal(unix_time(date_utc), date_unix)
  expect_equal(unix_time(date_nz), date_unix)
  expect_equal(unix_time(date_normal), date_unix)
  
  
  # Check the tz handling
  expect_equal(date_normal, as.POSIXct(date_unix, origin = "1970-01-01"))
  
  # Time zone change for function
  expect_equal(unix_time(date_normal, tz = "Europe/London"), 1437892806)
  expect_equal(unix_time(date_normal, tz = "UTC"), 1437896406)
  expect_equal(unix_time(date_normal, tz = "Pacific/Auckland"), 1437853206)
  
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

