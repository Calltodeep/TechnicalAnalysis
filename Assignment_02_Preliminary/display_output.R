library(ggplot2)
source("stock_functions.R")

# Display utilities for loaded stock data frames and computed statistics
display_stock_outputs <- function(portfolio_file = "portfolio.txt", ma_n = 20) {
  stocks_list <- load_stock_data(portfolio_file)
  stats_list <- list()

  for (sym in names(stocks_list)) {
    df <- stocks_list[[sym]]
    cat("\n==================================================\n")
    cat("Stock Symbol:", sym, "| Rows:", nrow(df), "\n")
    cat("==================================================\n")
    print(head(df, 5))

    sym_stats <- calculate_statistics(df, ma_n = ma_n)
    sym_stats$Symbol <- sym
    stats_list[[sym]] <- sym_stats
  }

  summary_table <- do.call(rbind, stats_list)
  rownames(summary_table) <- NULL
  summary_table <- summary_table[, c("Symbol", "Latest_Close", "Moving_Avg_20d", "Mean", "Median", "Mode", "Std_Dev")]

  cat("\n--- Summary Statistics Across Portfolio ---\n")
  print(summary_table)

  # Visualization 1: Closing Price + Moving Average for each stock
  for (sym in names(stocks_list)) {
    df <- stocks_list[[sym]]
    close_col <- grep("Close", names(df), value = TRUE, ignore.case = TRUE)[1]
    df$Close_Val <- as.numeric(df[[close_col]])
    df$SMA_Val <- TTR::SMA(df$Close_Val, n = ma_n)

    p <- ggplot(df, aes(x = as.Date(Date))) +
      geom_line(aes(y = Close_Val, color = "Close Price"), linewidth = 0.8) +
      geom_line(aes(y = SMA_Val, color = paste0(ma_n, "-Day SMA")), linewidth = 0.8, linetype = "dashed") +
      labs(
        title = paste(sym, "Closing Price &", ma_n, "-Day Moving Average"),
        x = "Date",
        y = "Price",
        color = "Series"
      ) +
      theme_minimal()

    print(p)
  }

  # Visualization 2: Comparison of Mean vs. Median vs. Std Dev across stocks
  p_bar <- ggplot(summary_table, aes(x = Symbol, y = Mean, fill = Symbol)) +
    geom_col(show.legend = FALSE) +
    geom_errorbar(aes(ymin = Mean - Std_Dev, ymax = Mean + Std_Dev), width = 0.2) +
    labs(
      title = "Portfolio Mean Closing Price (±1 Std Dev)",
      x = "Stock Symbol",
      y = "Price"
    ) +
    theme_minimal()

  print(p_bar)

  invisible(list(data = stocks_list, statistics = summary_table))
}

# Run when executing script
results <- display_stock_outputs("portfolio.txt", ma_n = 20)
