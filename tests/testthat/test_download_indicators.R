context("Download Indicators")

# contants
expected_columns <- c("Indicator", "Date", "Value")

# data
cdi_daily <- download_indicator_cdi_daily()
cdi_monthly <- download_indicator_cdi_monthly()
ibovespa_daily <- download_indicator_ibovespa_daily()
ipca_accumulated <- download_indicator_ipca_accumulated()
ipca_monthly <- download_indicator_ipca_monthly()
selic_daily <- download_indicator_selic_daily()
selic_monthly <- download_indicator_selic_monthly()
tjlp_monthly <- download_indicator_tjlp_monthly()

# tests
test_that("CDI indicator have all expected columns", {
  expect_equal(colnames(cdi_daily), expected_columns)
  expect_equal(colnames(cdi_monthly), expected_columns)
})

test_that("IBOVESPA indicator have all expected columns", {
  expect_equal(colnames(ibovespa_daily), expected_columns)
})

test_that("IPCA indicator have all expected columns", {
  expect_equal(colnames(ipca_accumulated), expected_columns)
  expect_equal(colnames(ipca_monthly), expected_columns)
})

test_that("SELIC indicator have all expected columns", {
  expect_equal(colnames(selic_daily), expected_columns)
  expect_equal(colnames(tjlp_monthly), expected_columns)
})

test_that("TJKP indicator have all expected columns", {
  expect_equal(colnames(tjlp_monthly), expected_columns)
})
