context("Download Stock Prices")

# data
ticker.data <- DownloadStockPrices("ITUB3")

# tests
test_that("Stock prices have all expected columns", {
  expect_equal(colnames(ticker.data), c("Ticker", "Date", "Open", "High", "Low", "Close", "Adjusted", "Volume"))
})
