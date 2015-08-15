context("Date things")

test_that("round_date_interval at different intervals", {
  x <- lubridate::ymd_hms("2015-06-03 16:06:08")
  
  expect_identical(round_date_interval(x, "min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "2 min"), 
                   lubridate::ymd_hms("2015-06-03 16:06:00"))
  
  expect_identical(round_date_interval(x, "5 min"), 
                   lubridate::ymd_hms("2015-06-03 16:05:00"))
  
  expect_identical(round_date_interval(x, "10 min"), 
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
  date.unix <- 1437896406
  # This is 2015-07-26 07:40:06 UTC
  
  # Parse the unix time into dates with tz info
  date.london <- as.POSIXct(date.unix, origin = "1970-01-01", tz = "Europe/London")
  date.utc <- as.POSIXct(date.unix, origin = "1970-01-01", tz = "UTC")
  date.nz <- as.POSIXct(date.unix, origin = "1970-01-01", tz = "Pacific/Auckland")
  
  # How I parse dates
  date.normal <- lubridate::ymd_hms("2015-07-26 07:40:06")
  
  # Only testing the base as.numeric function
  expect_equal(unix_time(date.london), date.unix)
  expect_equal(unix_time(date.utc), date.unix)
  expect_equal(unix_time(date.nz), date.unix)
  expect_equal(unix_time(date.normal), date.unix)
  
  
  # Check the tz handling
  expect_equal(date.normal, as.POSIXct(date.unix, origin = "1970-01-01"))
  
  # Time zone change for function
  expect_equal(unix_time(date.normal, tzone = "Europe/London"), 1437892806)
  expect_equal(unix_time(date.normal, tzone = "UTC"), 1437896406)
  expect_equal(unix_time(date.normal, tzone = "Pacific/Auckland"), 1437853206)
  
})


context("Test the low-level utility functions")

test_that("Test hexidecimal to integer function", {
  
  x <- "fff4d"
  # As binary: threadr::as.binary(strtoi(x, 16))
  expect_equal(strtoi(x, 16), 1048397)
  
})


test_that("Test Two's Complement function", {
  
  x <- 1048397
  expect_identical(threadr::twos_complement(x), -179)
  
})

