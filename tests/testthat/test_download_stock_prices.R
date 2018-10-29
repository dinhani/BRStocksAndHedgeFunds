context("Download Stock Prices")

# data
ticker_data <- download_stock_prices("ITUB3")

# tests
test_that("Stock prices have all expected columns", {
  expect_equal(colnames(ticker_data), c("Ticker", "Date", "Open", "High", "Low", "Close", "Adjusted", "Volume"))
})
