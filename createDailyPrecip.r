#' Aggregate Sub-daily Precipitation Data to Daily Totals
#'
#' This script reads a CSV file containing sub-daily precipitation data
#' and aggregates it to daily precipitation totals.
#'
#' @param input_file Path to the input CSV file (default: "short.csv")
#' @param output_file Path to the output CSV file (default: "daily_precipitation.csv")
#' @param threshold Minimum threshold for precipitation (default: 0.1 mm)
#' @return A data frame with Date and daily Precipitation totals

aggregate_daily_precipitation <- function(input_file = "precip.csv", 
                                        output_file = "daily_precipitation.csv",
                                        threshold = 0.1) {
  
  # Load required library
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  
  # Read the input CSV file
  cat("Reading input file:", input_file, "\n")
  data <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # Check if required columns exist
  if (!all(c("date", "Precipitation") %in% names(data))) {
    stop("Input file must contain 'date' and 'Precipitation' columns")
  }
  
  # Convert date column to POSIXct (handles YYYY-MM-DD HH24:MI format)
  cat("Converting date format...\n")
  data$datetime <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M")
  
  # Check if date conversion was successful
  if (sum(is.na(data$datetime)) > 0) {
    warning(paste("Failed to parse", sum(is.na(data$datetime)), "dates. Check date format."))
  }
  
  # Extract date only (YYYY-MM-DD format)
  data$date_only <- as.Date(data$datetime)
  
  # Remove rows with invalid dates
  data <- data[!is.na(data$date_only), ]
  
  # Aggregate precipitation by date (sum daily totals)
  cat("Aggregating precipitation by date...\n")
  daily_precip <- aggregate(Precipitation ~ date_only, data = data, FUN = sum, na.rm = TRUE)
  
  # Rename columns for clarity
  names(daily_precip) <- c("Date", "Precipitation")
  
  # Sort by date
  daily_precip <- daily_precip[order(daily_precip$Date), ]
  
  # Apply threshold and rounding
  cat("Applying threshold (", threshold, " mm) and rounding...\n")
  daily_precip$Precipitation <- ifelse(daily_precip$Precipitation >= threshold,
                                      round(daily_precip$Precipitation, 1),
                                      0)
  
  # Write results to output file
  write.csv(daily_precip, output_file, row.names = TRUE)
  
  # Print summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Input records:", nrow(data), "\n")
  cat("Output daily records:", nrow(daily_precip), "\n")
  cat("Date range:", as.character(min(daily_precip$Date)), "to", as.character(max(daily_precip$Date)), "\n")
  cat("Total precipitation:", round(sum(daily_precip$Precipitation, na.rm = TRUE), 2), "\n")
  cat("Average daily precipitation:", round(mean(daily_precip$Precipitation, na.rm = TRUE), 2), "\n")
  cat("Days with precipitation >= threshold (", threshold, " mm):", sum(daily_precip$Precipitation > 0, na.rm = TRUE), "\n")
  cat("Days set to 0 (below threshold):", sum(daily_precip$Precipitation == 0, na.rm = TRUE), "\n")
  cat("Threshold used:", threshold, " mm\n")
  cat("Output saved to:", output_file, "\n")
  
  return(daily_precip)
}

# Example usage:
# Use default file names
# result <- aggregate_daily_precipitation()

# Use custom file names and threshold
# result <- aggregate_daily_precipitation("my_input_file.csv", "my_daily_output.csv", 0.2)

# Run the function with default parameters
cat("Starting precipitation aggregation...\n")
result <- aggregate_daily_precipitation()