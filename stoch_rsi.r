# Ensure rsi() and sma() are available if stoch_rsi.R is sourced standalone
if (!exists("rsi")) {
  source("rsi.R")
}
if (!exists("sma")) {
  source("sma.R")
}

# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # Remove leading NA values so min, max, and sma calculate properly
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  # Calculate the StochRSI
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  k_values <- (valid_rsi - min_rsi) / (max_rsi - min_rsi)
  
  # Calculate the %K line (SMA of StochRSI)
  k_line <- sma(k_values, k_period)
  
  # Calculate the %D line (SMA of %K)
  d_line <- sma(k_line, d_period)
  
  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}