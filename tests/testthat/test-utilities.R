# Test utilities functions
library(testthat)
library(internet_structure)

# Test IP validation
test_that("IP validation works correctly", {
  expect_true(validate_ip("192.168.1.1"))
  expect_true(validate_ip("8.8.8.8"))
  expect_false(validate_ip("256.1.1.1"))
  expect_false(validate_ip("invalid"))
  expect_false(validate_ip(""))
})

# Test IP to numeric conversion
test_that("IP to numeric conversion works", {
  expect_equal(ip_to_numeric("192.168.1.1"), 3232235777)
  expect_equal(ip_to_numeric("0.0.0.0"), 0)
  expect_equal(ip_to_numeric("255.255.255.255"), 4294967295)
  expect_true(is.na(ip_to_numeric("invalid")))
})

# Test numeric to IP conversion
test_that("Numeric to IP conversion works", {
  expect_equal(numeric_to_ip(3232235777), "192.168.1.1")
  expect_equal(numeric_to_ip(0), "0.0.0.0")
  expect_equal(numeric_to_ip(4294967295), "255.255.255.255")
  expect_true(is.na(numeric_to_ip(-1)))
  expect_true(is.na(numeric_to_ip(4294967296)))
})

# Test ASN validation
test_that("ASN validation works", {
  expect_true(validate_asn(15169))
  expect_true(validate_asn(1))
  expect_true(validate_asn(4294967295))
  expect_false(validate_asn(0))
  expect_false(validate_asn(-1))
  expect_false(validate_asn(4294967296))
  expect_false(validate_asn("invalid"))
})

# Test CIDR prefix calculation
test_that("CIDR prefix calculation works", {
  expect_equal(calculate_cidr_prefix("192.168.1.0", "192.168.1.255"), 24)
  expect_equal(calculate_cidr_prefix("10.0.0.0", "10.255.255.255"), 8)
  expect_true(is.na(calculate_cidr_prefix("invalid", "192.168.1.1")))
})

# Test AS information formatting
test_that("AS information formatting works", {
  expect_equal(format_as_info(15169, "Google LLC", "US"),
               "AS15169 (Google LLC, US)")
  expect_equal(format_as_info(15169), "AS15169")
  expect_equal(format_as_info(15169, country = "US"), "AS15169 (US)")
})
