#' Apply Daily Stretch Ratios to Sub-daily Precipitation Data
#'
#' This script reads daily stretch ratios from one CSV file and applies them
#' to sub-daily precipitation measurements in another CSV file.
#'
#' @param daily_file Path to the CSV file with daily stretch ratios (default: "shortStretch.csv")
#' @param subdaily_file Path to the CSV file with sub-daily precipitation data (default: "short.csv")
#' @param output_file Path to the output CSV file (default: "stretched_subdaily_precipitation.csv")
#' @return A data frame with datetime, original precipitation, stretched precipitation, and stretch ratio

apply_stretch_ratios <- function(daily_file = "precipitation_stretched.csv",
                                subdaily_file = "precip.csv", 
                                output_file = "stretched_subdaily_precipitation.csv") {
  
  # Load required library
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  
  # Read the daily stretch ratios file
  cat("Reading daily stretch ratios from:", daily_file, "\n")
  daily_data <- read.csv(daily_file, stringsAsFactors = FALSE)
  
  # Check if required columns exist in daily file
  if (!all(c("Date", "StretchRatio") %in% names(daily_data))) {
    stop("Daily file must contain 'Date' and 'StretchRatio' columns")
  }
  
  # Convert Date column to Date format in daily data
  daily_data$Date <- as.Date(daily_data$Date)
  
  # Read the sub-daily precipitation file
  cat("Reading sub-daily precipitation from:", subdaily_file, "\n")
  subdaily_data <- read.csv(subdaily_file, stringsAsFactors = FALSE)
  
  # Check if required columns exist in sub-daily file
  if (!all(c("date", "Precipitation") %in% names(subdaily_data))) {
    stop("Sub-daily file must contain 'date' and 'Precipitation' columns")
  }
  
  # Convert datetime column to POSIXct (handles YYYY-MM-DD HH24:MI format)
  cat("Converting datetime format...\n")
  subdaily_data$datetime <- as.POSIXct(subdaily_data$date, format = "%Y-%m-%d %H:%M")
  
  # Check if date conversion was successful
  if (sum(is.na(subdaily_data$datetime)) > 0) {
    warning(paste("Failed to parse", sum(is.na(subdaily_data$datetime)), "dates. Check date format."))
  }
  
  # Extract date only (YYYY-MM-DD format) for matching
  subdaily_data$date_only <- as.Date(subdaily_data$datetime)
  
  # Remove rows with invalid dates
  subdaily_data <- subdaily_data[!is.na(subdaily_data$date_only), ]
  
  # Merge sub-daily data with daily stretch ratios
  cat("Merging data and applying stretch ratios...\n")
  merged_data <- merge(subdaily_data, daily_data, by.x = "date_only", by.y = "Date", all.x = TRUE)
  
  # Initialize stretched precipitation column
  merged_data$StretchedPrecipitation <- merged_data$Precipitation
  
  # Apply stretch ratios only to non-zero precipitation values
  non_zero_mask <- merged_data$Precipitation > 0 & !is.na(merged_data$StretchRatio)
  merged_data$StretchedPrecipitation[non_zero_mask] <- 
    merged_data$Precipitation[non_zero_mask] * merged_data$StretchRatio[non_zero_mask]
  
  # Create final output dataframe with desired columns
  result <- data.frame(
    DateTime = merged_data$date,
    OriginalPrecipitation = merged_data$Precipitation,
    StretchedPrecipitation = merged_data$StretchedPrecipitation,
    DailyStretchRatio = merged_data$StretchRatio
  )
  
  # Sort by datetime
  result <- result[order(as.POSIXct(result$DateTime, format = "%Y-%m-%d %H:%M")), ]
  
  # Handle missing stretch ratios
  missing_ratios <- sum(is.na(result$DailyStretchRatio))
  if (missing_ratios > 0) {
    warning(paste("Warning:", missing_ratios, "records have missing stretch ratios. These will use original precipitation values."))
    # For missing stretch ratios, keep original precipitation and set ratio to 1.0
    result$DailyStretchRatio[is.na(result$DailyStretchRatio)] <- 1.0
    result$StretchedPrecipitation[is.na(result$StretchedPrecipitation)] <- 
      result$OriginalPrecipitation[is.na(result$StretchedPrecipitation)]
  }
  
  # Write results to output file
  write.csv(result, output_file, row.names = TRUE)
  
  # Print summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Sub-daily input records:", nrow(subdaily_data), "\n")
  cat("Daily stretch ratio records:", nrow(daily_data), "\n")
  cat("Output records:", nrow(result), "\n")
  cat("Records with non-zero precipitation:", sum(result$OriginalPrecipitation > 0, na.rm = TRUE), "\n")
  cat("Records with stretch ratios applied:", sum(result$OriginalPrecipitation > 0 & result$DailyStretchRatio != 1.0, na.rm = TRUE), "\n")
  cat("Date range:", as.character(min(as.Date(merged_data$date_only), na.rm = TRUE)), 
      "to", as.character(max(as.Date(merged_data$date_only), na.rm = TRUE)), "\n")
  cat("Total original precipitation:", round(sum(result$OriginalPrecipitation, na.rm = TRUE), 2), "\n")
  cat("Total stretched precipitation:", round(sum(result$StretchedPrecipitation, na.rm = TRUE), 2), "\n")
  cat("Average stretch ratio (non-zero precip):", 
      round(mean(result$DailyStretchRatio[result$OriginalPrecipitation > 0], na.rm = TRUE), 3), "\n")
  cat("Output saved to:", output_file, "\n")
  
  return(result)
}

# Example usage:
# Use default file names
# result <- apply_stretch_ratios()

# Use custom file names
# result <- apply_stretch_ratios("my_daily_stretch.csv", "my_subdaily_data.csv", "my_output.csv")

# Run the function with default parameters
cat("Starting precipitation stretching process...\n")
result <- apply_stretch_ratios()