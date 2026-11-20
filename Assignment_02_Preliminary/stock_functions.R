library(quantmod)
library(TTR)

# Helper function to compute statistical mode for numeric vectors
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  freq_table <- table(x)
  as.numeric(names(freq_table)[which.max(freq_table)])
}

# 5. Function to read portfolio.txt and load each stock as a separate data frame
load_stock_data <- function(file_path = "portfolio.txt",
                            from_date = Sys.Date() - 365,
                            to_date = Sys.Date()) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  symbols <- readLines(file_path, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[nzchar(symbols)]

  stock_dfs <- list()

  for (sym in symbols) {
    tryCatch({
      xt <- getSymbols(sym, src = "yahoo", from = from_date, to = to_date, auto.assign = FALSE)
      df <- data.frame(
        Date = index(xt),
        coredata(xt),
        stringsAsFactors = FALSE
      )
      names(df) <- sub(paste0("^", make.names(sym), "\\."), "", names(df))
      stock_dfs[[sym]] <- df
    }, error = function(e) {
      warning(paste("Could not load data for symbol:", sym, "-", e$message))
    })
  }

  return(stock_dfs)
}

# 6. Function to compute Moving Average, Mean, Mode, Median, and Standard Deviation
calculate_statistics <- function(stock_df, ma_n = 20) {
  close_col <- grep("Close", names(stock_df), value = TRUE, ignore.case = TRUE)[1]
  if (is.na(close_col)) {
    stop("No Close price column found in data frame.")
  }

  prices <- as.numeric(stock_df[[close_col]])
  prices_clean <- prices[!is.na(prices)]

  sma_vec <- TTR::SMA(prices, n = ma_n)
  latest_sma <- tail(sma_vec[!is.na(sma_vec)], 1)
  if (length(latest_sma) == 0) latest_sma <- NA_real_

  stats_df <- data.frame(
    Latest_Close = tail(prices_clean, 1),
    Moving_Avg_20d = as.numeric(latest_sma),
    Mean = mean(prices_clean),
    Mode = get_mode(prices_clean),
    Median = median(prices_clean),
    Std_Dev = sd(prices_clean),
    stringsAsFactors = FALSE
  )

  return(stats_df)
}
