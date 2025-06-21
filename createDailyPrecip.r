#' Aggregate Sub-daily Precipitation Data to Daily Totals
#'
#' This script reads a CSV file containing sub-daily precipitation data
#' and aggregates it to daily precipitation totals. Creates a companion JSON
#' metadata file with the same base name as the output CSV.
#'
#' @param input_file Path to the input CSV file (default: "short.csv")
#' @param output_file Path to the output CSV file (default: "daily_precipitation.csv")
#' @param threshold Minimum threshold for precipitation (default: 0.1 mm)
#' @return A data frame with Date and daily Precipitation totals

aggregate_daily_precipitation <- function(input_file = "short.csv", 
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
  write.csv(daily_precip, output_file, row.names = FALSE)
  
  # Create JSON metadata file with same base name as CSV
  json_file <- sub("\\.csv$", ".json", output_file)
  
  # Calculate additional summary statistics
  max_precip <- max(daily_precip$Precipitation, na.rm = TRUE)
  min_precip <- min(daily_precip$Precipitation, na.rm = TRUE)
  median_precip <- median(daily_precip$Precipitation, na.rm = TRUE)
  std_dev_precip <- sd(daily_precip$Precipitation, na.rm = TRUE)
  
  # Calculate percentiles for non-zero precipitation days
  non_zero_precip <- daily_precip$Precipitation[daily_precip$Precipitation > 0]
  percentiles <- if(length(non_zero_precip) > 0) {
    list(
      p25 = quantile(non_zero_precip, 0.25, na.rm = TRUE),
      p50 = quantile(non_zero_precip, 0.50, na.rm = TRUE),
      p75 = quantile(non_zero_precip, 0.75, na.rm = TRUE),
      p90 = quantile(non_zero_precip, 0.90, na.rm = TRUE),
      p95 = quantile(non_zero_precip, 0.95, na.rm = TRUE),
      p99 = quantile(non_zero_precip, 0.99, na.rm = TRUE)
    )
  } else {
    list(p25 = NA, p50 = NA, p75 = NA, p90 = NA, p95 = NA, p99 = NA)
  }
  
  # Prepare comprehensive metadata
  metadata <- list(
    parameters = list(
      input_file = input_file,
      output_file = output_file,
      threshold_mm = threshold
    ),
    results = list(
      input_records = nrow(data),
      output_daily_records = nrow(daily_precip),
      date_range_start = as.character(min(daily_precip$Date)),
      date_range_end = as.character(max(daily_precip$Date)),
      total_precipitation_mm = round(sum(daily_precip$Precipitation, na.rm = TRUE), 2),
      average_daily_precipitation_mm = round(mean(daily_precip$Precipitation, na.rm = TRUE), 2),
      median_daily_precipitation_mm = round(median_precip, 2),
      max_daily_precipitation_mm = round(max_precip, 2),
      min_daily_precipitation_mm = round(min_precip, 2),
      std_dev_precipitation_mm = round(std_dev_precip, 2),
      days_with_precipitation = sum(daily_precip$Precipitation > 0, na.rm = TRUE),
      days_set_to_zero = sum(daily_precip$Precipitation == 0, na.rm = TRUE),
      percentage_dry_days = round((sum(daily_precip$Precipitation == 0, na.rm = TRUE) / nrow(daily_precip)) * 100, 1),
      percentage_wet_days = round((sum(daily_precip$Precipitation > 0, na.rm = TRUE) / nrow(daily_precip)) * 100, 1),
      precipitation_percentiles = percentiles
    ),
    generation_info = list(
      date_generated = as.character(Sys.Date()),
      timestamp_generated = as.character(Sys.time()),
      r_version = R.version.string,
      script_name = "createDailyPrecip.r",
      function_name = "aggregate_daily_precipitation"
    )
  )
  
  # Write JSON metadata file
  cat("Creating JSON metadata file...\n")
  if(require(jsonlite, quietly = TRUE)) {
    jsonlite::write_json(metadata, json_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Metadata saved to:", json_file, "\n")
  } else {
    # Fallback: write JSON manually if jsonlite is not available
    cat("jsonlite package not available, writing JSON manually...\n")
    
    # Helper function to format list as JSON
    format_percentiles <- function(p) {
      paste0("{\n",
             "      \"p25\": ", ifelse(is.na(p$p25), "null", round(p$p25, 2)), ",\n",
             "      \"p50\": ", ifelse(is.na(p$p50), "null", round(p$p50, 2)), ",\n",
             "      \"p75\": ", ifelse(is.na(p$p75), "null", round(p$p75, 2)), ",\n",
             "      \"p90\": ", ifelse(is.na(p$p90), "null", round(p$p90, 2)), ",\n",
             "      \"p95\": ", ifelse(is.na(p$p95), "null", round(p$p95, 2)), ",\n",
             "      \"p99\": ", ifelse(is.na(p$p99), "null", round(p$p99, 2)), "\n",
             "    }")
    }
    
    json_content <- paste0(
      "{\n",
      "  \"parameters\": {\n",
      "    \"input_file\": \"", input_file, "\",\n",
      "    \"output_file\": \"", output_file, "\",\n",
      "    \"threshold_mm\": ", threshold, "\n",
      "  },\n",
      "  \"results\": {\n",
      "    \"input_records\": ", nrow(data), ",\n",
      "    \"output_daily_records\": ", nrow(daily_precip), ",\n",
      "    \"date_range_start\": \"", as.character(min(daily_precip$Date)), "\",\n",
      "    \"date_range_end\": \"", as.character(max(daily_precip$Date)), "\",\n",
      "    \"total_precipitation_mm\": ", round(sum(daily_precip$Precipitation, na.rm = TRUE), 2), ",\n",
      "    \"average_daily_precipitation_mm\": ", round(mean(daily_precip$Precipitation, na.rm = TRUE), 2), ",\n",
      "    \"median_daily_precipitation_mm\": ", round(median_precip, 2), ",\n",
      "    \"max_daily_precipitation_mm\": ", round(max_precip, 2), ",\n",
      "    \"min_daily_precipitation_mm\": ", round(min_precip, 2), ",\n",
      "    \"std_dev_precipitation_mm\": ", round(std_dev_precip, 2), ",\n",
      "    \"days_with_precipitation\": ", sum(daily_precip$Precipitation > 0, na.rm = TRUE), ",\n",
      "    \"days_set_to_zero\": ", sum(daily_precip$Precipitation == 0, na.rm = TRUE), ",\n",
      "    \"percentage_dry_days\": ", round((sum(daily_precip$Precipitation == 0, na.rm = TRUE) / nrow(daily_precip)) * 100, 1), ",\n",
      "    \"percentage_wet_days\": ", round((sum(daily_precip$Precipitation > 0, na.rm = TRUE) / nrow(daily_precip)) * 100, 1), ",\n",
      "    \"precipitation_percentiles\": ", format_percentiles(percentiles), "\n",
      "  },\n",
      "  \"generation_info\": {\n",
      "    \"date_generated\": \"", as.character(Sys.Date()), "\",\n",
      "    \"timestamp_generated\": \"", as.character(Sys.time()), "\",\n",
      "    \"r_version\": \"", R.version.string, "\",\n",
      "    \"script_name\": \"createDailyPrecip.r\",\n",
      "    \"function_name\": \"aggregate_daily_precipitation\"\n",
      "  }\n",
      "}"
    )
    writeLines(json_content, json_file)
    cat("Metadata saved to:", json_file, " (manual JSON format)\n")
  }
  
  # Print summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Input records:", nrow(data), "\n")
  cat("Output daily records:", nrow(daily_precip), "\n")
  cat("Date range:", as.character(min(daily_precip$Date)), "to", as.character(max(daily_precip$Date)), "\n")
  cat("Total precipitation:", round(sum(daily_precip$Precipitation, na.rm = TRUE), 2), "mm\n")
  cat("Average daily precipitation:", round(mean(daily_precip$Precipitation, na.rm = TRUE), 2), "mm\n")
  cat("Maximum daily precipitation:", round(max_precip, 2), "mm\n")
  cat("Standard deviation:", round(std_dev_precip, 2), "mm\n")
  cat("Days with precipitation >= threshold (", threshold, " mm):", sum(daily_precip$Precipitation > 0, na.rm = TRUE), "\n")
  cat("Days set to 0 (below threshold):", sum(daily_precip$Precipitation == 0, na.rm = TRUE), "\n")
  cat("Percentage of dry days:", round((sum(daily_precip$Precipitation == 0, na.rm = TRUE) / nrow(daily_precip)) * 100, 1), "%\n")
  cat("Threshold used:", threshold, "mm\n")
  cat("Output CSV saved to:", output_file, "\n")
  cat("Output JSON saved to:", json_file, "\n")
  
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