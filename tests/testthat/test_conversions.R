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
