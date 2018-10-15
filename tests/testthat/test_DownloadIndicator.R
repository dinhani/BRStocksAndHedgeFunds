context("Download Indicators")

# contants
expected.columns <- c("Indicator", "Date", "Value")

# data
cdi.daily <- DownloadIndicatorCDIDaily()
cdi.monthly <- DownloadIndicatorCDIMonthly()
ibovespa.daily <- DownloadIndicatorIbovespaDaily()
ipca.accumulated <- DownloadIndicatorIpcaAccumulated()
ipca.monthly <- DownloadIndicatorIpcaMonthly()
selic.daily <- DownloadIndicatorSelicDaily()
selic.monthly <- DownloadIndicatorSelicMonthly()
tjlp.monthly <- DownloadIndicatorTjlpMonthly()

# tests
test_that("CDI indicator have all expected columns", {
  expect_equal(colnames(cdi.daily), expected.columns)
  expect_equal(colnames(cdi.monthly), expected.columns)
})

test_that("IBOVESPA indicator have all expected columns", {
  expect_equal(colnames(ibovespa.daily), expected.columns)
})

test_that("IPCA indicator have all expected columns", {
  expect_equal(colnames(ipca.accumulated), expected.columns)
  expect_equal(colnames(ipca.monthly), expected.columns)
})

test_that("SELIC indicator have all expected columns", {
  expect_equal(colnames(selic.daily), expected.columns)
  expect_equal(colnames(tjlp.monthly), expected.columns)
})

test_that("TJKP indicator have all expected columns", {
  expect_equal(colnames(tjlp.monthly), expected.columns)
})
