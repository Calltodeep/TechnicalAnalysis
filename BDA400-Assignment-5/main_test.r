# Load all 9 indicator scripts
source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

# 1. Create sample stock data frame (Date, Open, High, Low, Close, Volume)
stock_df <- data.frame(
  Date   = seq(as.Date("2026-01-01"), by = "day", length.out = 25),
  Open   = c(150, 152, 151, 154, 156, 155, 158, 160, 159, 162,
             165, 164, 163, 161, 160, 158, 157, 159, 162, 164,
             167, 169, 168, 171, 174),
  High   = c(153, 154, 153, 157, 158, 157, 161, 162, 161, 165,
             167, 166, 165, 163, 162, 160, 159, 161, 164, 167,
             170, 171, 170, 174, 176),
  Low    = c(149, 150, 150, 153, 154, 154, 157, 158, 158, 161,
             163, 162, 161, 159, 158, 156, 155, 158, 160, 163,
             166, 167, 166, 170, 172),
  Close  = c(152, 151, 153, 156, 155, 158, 160, 159, 162, 165,
             164, 163, 161, 160, 158, 157, 159, 162, 164, 167,
             169, 168, 171, 174, 175),
  Volume = c(1200, 1350, 1100, 1600, 1450, 1700, 1850, 1500, 1900, 2100,
             1750, 1600, 1550, 1400, 1650, 1800, 1950, 2050, 2200, 2400,
             2300, 2150, 2500, 2750, 2900)
)

# 2. Apply Data Frame Filter (e.g., filter dates from Jan 5 onward & high volume)
filtered_df <- subset(stock_df, Date >= as.Date("2026-01-05") & Volume > 1400)
close_prices <- filtered_df$Close

# 3. Test All 9 Technical Indicators
cat("--- 1. SMA (Period = 5) ---\n")
print(sma(close_prices, period = 5))

cat("\n--- 2. EMA (Period = 5) ---\n")
ema_short <- ema(close_prices, period = 5)
ema_long  <- ema(close_prices, period = 10)
print(ema_short)

cat("\n--- 3. MACD (5, 10, 3) ---\n")
macd_res <- macd(close_prices, short_period = 5, long_period = 10, signal_period = 3)
print(macd_res)

cat("\n--- 4. Standard Deviation ---\n")
print(stdev(close_prices))

cat("\n--- 5. Linear Regression (Length = 10, Offset = 0) ---\n")
print(linreg(close_prices, regressionLength = 10, regressionOffset = 0))

cat("\n--- 6. RSI (Period = 6) ---\n")
rsi_res <- rsi(close_prices, period = 6)
print(rsi_res)

cat("\n--- 7. Stochastic RSI (RSI = 6, K = 3, D = 3) ---\n")
print(stoch_rsi(close_prices, period = 6, k_period = 3, d_period = 3))

cat("\n--- 8. Crossover (EMA 5 vs EMA 10) ---\n")
cross_up <- crossover(ema_short, ema_long)
print(cross_up)

cat("\n--- 9. Crossunder (EMA 5 vs EMA 10) ---\n")
cross_down <- crossunder(ema_short, ema_long)
print(cross_down)

# 4. Apply Trading Rules (Buy on Crossover "Up", Sell on Crossunder "True")
filtered_df$EMA_5 <- ema_short
filtered_df$EMA_10 <- ema_long
filtered_df$RSI <- rsi_res
filtered_df$Signal <- ifelse(cross_up == "Up", "BUY",
                      ifelse(cross_down == "True", "SELL", "HOLD"))

cat("\n--- Trading Signals Summary ---\n")
print(filtered_df[, c("Date", "Close", "EMA_5", "EMA_10", "RSI", "Signal")])