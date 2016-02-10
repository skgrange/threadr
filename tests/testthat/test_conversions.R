context("Conversion functions")

test_that("Gas conversions", {
  
  # http://www.apis.ac.uk/unit-conversion
  expect_equal(gas_volume_to_mass(400, gas = "o3"), 856.760, tolerance = 0.001)
  expect_equal(gas_mass_to_volume(800, gas = "o3"), 373.5)
  
  expect_equal(gas_volume_to_mass(3.52734, gas = "nox"), 7.240, tolerance = 0.002)
  expect_equal(gas_volume_to_mass(3.52734, gas = "nox", temperature = 20), 
               3.52734 * 1.912, tolerance = 0.003)
  
  expect_equal(gas_mass_to_volume(6.966383, gas = "no2"), 3.394, tolerance = 0.001)
  
})


test_that("Easy conversions", {
  
  expect_equal(miles_to_km(3), 1.609344 * 3)
  expect_equal(km_to_miles(6.5), 4.03891, tolerance = 0.0001)
  
  expect_equal(n_mile_to_km(2), 1.852 * 2)
  expect_equal(km_to_n_mile(5), 2.69978, tolerance = 0.0001)
  
})


# test_that("time_pad testing", {
#   
#   expect_equal(miles_to_km(3), 1.609344 * 3)
#   expect_equal(km_to_miles(6.5), 4.03891, tolerance = 0.0001)
#   
#   
# })