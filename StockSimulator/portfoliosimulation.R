# install.packages("quantmod")
library(quantmod)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(tidyverse)

# Next, specify the stocks and time period for the simulation
stock_tickers <- c("AAPL", "GOOGL", "MSFT", "AMZN")
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2019-12-31")

# Use the getSymbols function to download historical stock data
getSymbols(stock_tickers, src = "yahoo", from = start_date, to = end_date)

# Use the Ad function to calculate the cumulative returns for each stock
# stock_returns <- lapply(stock_tickers, function(x) Ad(get(x)))
# stock_returns <- sapply(stock_tickers, function(x) dailyReturn(get(x), type = "arithmetic"))

stock_returns <- do.call(cbind, lapply(stock_tickers, function(x) dailyReturn(get(x), type = "arithmetic"))) %>% t()

# Create a portfolio by combining the stock returns and specifying the initial investment and allocation for each stock
# portfolio_returns <- Reduce("*", stock_returns * c(0.25, 0.25, 0.25, 0.25))

portfolio_returns <- cumsum(Reduce("+", stock_returns * c(0.25, 0.25, 0.25, 0.25)))
portfolio_returns <- colSums(apply(stock_returns, 2, cumsum) * c(0.25, 0.25, 0.25, 0.25))

portfolio_returns <- cumprod(1 + portfolio_returns)

# Plot the portfolio returns over time
plot(portfolio_returns, main = "Portfolio Simulation", ylab = "Returns")
chart.TimeSeries(portfolio_returns, main = "Portfolio Simulation", ylab = "Returns")

