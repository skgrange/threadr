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
  expect_equal(km_to_miles(6.5), 4.03891, tolerance = 0.00001)
  
  expect_equal(n_mile_to_km(2), 1.852 * 2)
  expect_equal(km_to_n_mile(5), 2.69978, tolerance = 0.00001)
  
})


test_that("Gallon conversions", {
  
  expect_equal(gallon_to_litre(1), 4.54609)
  expect_equal(gallon_to_litre(1, type = "us"), 3.78541, tolerance = 0.00001)
  
  expect_equal(litre_to_gallon(55), 12.0983, tolerance = 0.00001)
  expect_equal(litre_to_gallon(55, type = "us"), 14.529452, tolerance = 0.00001)
  
})


test_that("Fuel consumption conversions", {
  
  # https://www.unitjuggler.com/convert-fuelconsumption-from-lper100km-to-mpg.html
  # mpg
  expect_equal(mpg_to_km_l(38), 13.452235219032)
  expect_equal(mpg_to_km_l(38, type = "us"), 16.15546088464)
  
  expect_equal(mpg_to_l_100_km(40), 7.0620234075)
  expect_equal(mpg_to_l_100_km(40, type = "us"), 5.8803645825)
  
  expect_equal(l_100km_to_mpg(6), 47.08015605)
  expect_equal(l_100km_to_mpg(6, type = "us"), 39.20243055)
  
  expect_equal(l_100_km_to_km_l(6), 16.666666666667)
  
})


test_that("temperature conversions", {
  
  expect_equal(fahrenheit_to_celsius(55), 12.777777777778)
  expect_equal(celsius_to_fahrenheit(32.5), 90.5)
  
})
