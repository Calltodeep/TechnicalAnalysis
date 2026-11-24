# Simple Moving Average (SMA) function
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize a vector to store the SMA values
  num_windows <- length(data) - period + 1
  sma_values <- numeric(num_windows)
  
  # Calculate SMA for each window of 'period' data points
  for (i in 1:num_windows) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}