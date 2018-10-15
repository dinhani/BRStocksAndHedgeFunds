context("Download Indicators")

# data
cdi.daily <- DownloadIndicatorCDIDaily()
cdi.monthly <- DownloadIndicatorCDIMonthly()

# tests
test_that("CDI indicator have all columns expected", {
  expect_equal(colnames(cdi.daily), c("Indicator", "Date", "Value"))
  expect_equal(colnames(cdi.monthly), c("Indicator", "Date", "Value"))
})
