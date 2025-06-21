#' Apply Daily Stretch Ratios to Sub-daily Precipitation Data
#'
#' This script reads daily stretch ratios from one CSV file and applies them
#' to sub-daily precipitation measurements in another CSV file.
#'
#' @param daily_file Path to the CSV file with daily stretch ratios (default: "precipitation_stretched.csv")
#' @param subdaily_file Path to the CSV file with sub-daily precipitation data (default: "precip.csv")
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
  
  # Check if daily stretch ratios file exists
  if (!file.exists(daily_file)) {
    stop(paste("Daily stretch ratios file not found:", daily_file, 
               "\nPlease run stretch.r or stretch_mass_balanced.r first to create this file."))
  }
  
  # Check if sub-daily precipitation file exists
  if (!file.exists(subdaily_file)) {
    stop(paste("Sub-daily precipitation file not found:", subdaily_file))
  }
  
  # Read the daily stretch ratios file
  cat("Reading daily stretch ratios from:", daily_file, "\n")
  daily_data <- read.csv(daily_file, stringsAsFactors = FALSE)
  
  # Check if required columns exist in daily file
  if (!all(c("Date", "StretchRatio") %in% names(daily_data))) {
    stop("Daily file must contain 'Date' and 'StretchRatio' columns")
  }
  
  # We need both StretchRatio and Precipitation columns for the sign() operation
  required_cols <- c("Date", "StretchRatio")
  if ("Precipitation" %in% names(daily_data)) {
    required_cols <- c(required_cols, "Precipitation")
  } else {
    warning("Precipitation column not found in daily file. Cannot apply sign(daily_value) correction.")
  }
  
  # Select only the columns we need from daily data to avoid naming conflicts
  daily_data <- daily_data[, required_cols]
  
  # Check for associated JSON file to get threshold information
  json_file <- sub("\\.csv$", ".json", daily_file)
  threshold_value <- NA
  if (file.exists(json_file)) {
    cat("Reading threshold information from:", json_file, "\n")
    tryCatch({
      if (require(jsonlite, quietly = TRUE)) {
        json_data <- jsonlite::fromJSON(json_file)
        if ("parameters" %in% names(json_data) && "threshold" %in% names(json_data$parameters)) {
          threshold_value <- json_data$parameters$threshold
          cat("Found threshold value:", threshold_value, "\n")
        }
      } else {
        warning("jsonlite package not available. Cannot read threshold from JSON file.")
      }
    }, error = function(e) {
      warning(paste("Could not read JSON file:", e$message))
    })
  } else {
    cat("No JSON file found at:", json_file, "\n")
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
  
  # Handle column naming after merge (when both datasets have Precipitation columns)
  if ("Precipitation.x" %in% names(merged_data)) {
    # Rename columns for clarity
    names(merged_data)[names(merged_data) == "Precipitation.x"] <- "SubdailyPrecipitation"
    if ("Precipitation.y" %in% names(merged_data)) {
      names(merged_data)[names(merged_data) == "Precipitation.y"] <- "DailyPrecipitation"
    }
    precip_col <- "SubdailyPrecipitation"
    daily_precip_available <- "DailyPrecipitation" %in% names(merged_data)
  } else {
    # No column conflict - use original name
    precip_col <- "Precipitation"
    daily_precip_available <- "Precipitation" %in% names(daily_data)
  }
  
  # Initialize stretched precipitation column with original values
  merged_data$StretchedPrecipitation <- merged_data[[precip_col]]
  
  # Apply stretch ratios only to non-zero precipitation values with valid stretch ratios
  non_zero_mask <- merged_data[[precip_col]] > 0 & !is.na(merged_data$StretchRatio)
  merged_data$StretchedPrecipitation[non_zero_mask] <- 
    merged_data[[precip_col]][non_zero_mask] * merged_data$StretchRatio[non_zero_mask]
  
  # Apply sign(daily_value) correction if daily precipitation data is available
  if (daily_precip_available && "DailyPrecipitation" %in% names(merged_data)) {
    # Apply sign correction: multiply by sign of daily precipitation value
    # This ensures that days with 0 daily precipitation (below threshold) have 0 sub-daily values
    daily_sign <- sign(merged_data$DailyPrecipitation)
    merged_data$StretchedPrecipitation <- merged_data$StretchedPrecipitation * daily_sign
    
    cat("Applied sign(daily_value) correction to ensure zero sub-daily values when daily sum is zero\n")
  } else {
    cat("Warning: Daily precipitation values not available for sign() correction\n")
  }
  
  # Create final output dataframe with desired columns
  result <- data.frame(
    DateTime = merged_data$date,
    OriginalPrecipitation = merged_data[[precip_col]],
    StretchedPrecipitation = merged_data$StretchedPrecipitation,
    DailyStretchRatio = merged_data$StretchRatio
  )
  
  # Sort by datetime
  result <- result[order(as.POSIXct(result$DateTime, format = "%Y-%m-%d %H:%M")), ]
  
  # Handle missing stretch ratios
  missing_ratios <- sum(is.na(result$DailyStretchRatio))
  if (missing_ratios > 0) {
    cat("Warning:", missing_ratios, "records have missing stretch ratios.\n")
    cat("These records will keep original precipitation values and stretch ratio = 1.0\n")
    # For missing stretch ratios, keep original precipitation and set ratio to 1.0
    result$DailyStretchRatio[is.na(result$DailyStretchRatio)] <- 1.0
    result$StretchedPrecipitation[is.na(result$StretchedPrecipitation)] <- 
      result$OriginalPrecipitation[is.na(result$StretchedPrecipitation)]
  }
  
  # Write results to output file
  write.csv(result, output_file, row.names = FALSE)
  
  # Create JSON metadata file
  output_json_file <- sub("\\.csv$", ".json", output_file)
  
  # Prepare metadata
  metadata <- list(
    parameters = list(
      daily_file = daily_file,
      subdaily_file = subdaily_file,
      output_file = output_file,
      threshold_value = if (!is.na(threshold_value)) threshold_value else "not_available"
    ),
    results = list(
      subdaily_input_records = nrow(subdaily_data),
      daily_stretch_ratio_records = nrow(daily_data),
      output_records = nrow(result),
      records_with_nonzero_precipitation = sum(result$OriginalPrecipitation > 0, na.rm = TRUE),
      records_with_stretch_applied = sum(result$OriginalPrecipitation > 0 & result$DailyStretchRatio != 1.0, na.rm = TRUE),
      records_with_missing_ratios = missing_ratios,
      date_range_start = as.character(min(as.Date(merged_data$date_only), na.rm = TRUE)),
      date_range_end = as.character(max(as.Date(merged_data$date_only), na.rm = TRUE)),
      total_original_precipitation = round(sum(result$OriginalPrecipitation, na.rm = TRUE), 6),
      total_stretched_precipitation = round(sum(result$StretchedPrecipitation, na.rm = TRUE), 6),
      average_stretch_ratio_nonzero = round(mean(result$DailyStretchRatio[result$OriginalPrecipitation > 0], na.rm = TRUE), 6),
      mass_balance_ratio = round(sum(result$StretchedPrecipitation, na.rm = TRUE) / sum(result$OriginalPrecipitation, na.rm = TRUE), 6),
      sign_correction_applied = daily_precip_available && "DailyPrecipitation" %in% names(merged_data)
    ),
    generation_info = list(
      date_generated = Sys.Date(),
      timestamp_generated = Sys.time(),
      r_version = R.version.string,
      function_name = "apply_stretch_ratios"
    )
  )
  
  # Write JSON metadata file
  tryCatch({
    if (require(jsonlite, quietly = TRUE)) {
      jsonlite::write_json(metadata, output_json_file, pretty = TRUE, auto_unbox = TRUE)
      cat("Metadata saved to:", output_json_file, "\n")
    } else {
      # Fallback: write JSON manually if jsonlite is not available
      json_content <- paste0(
        "{\n",
        "  \"parameters\": {\n",
        "    \"daily_file\": \"", daily_file, "\",\n",
        "    \"subdaily_file\": \"", subdaily_file, "\",\n",
        "    \"output_file\": \"", output_file, "\",\n",
        "    \"threshold_value\": ", if (!is.na(threshold_value)) threshold_value else "\"not_available\"", "\n",
        "  },\n",
        "  \"results\": {\n",
        "    \"subdaily_input_records\": ", nrow(subdaily_data), ",\n",
        "    \"daily_stretch_ratio_records\": ", nrow(daily_data), ",\n",
        "    \"output_records\": ", nrow(result), ",\n",
        "    \"records_with_nonzero_precipitation\": ", sum(result$OriginalPrecipitation > 0, na.rm = TRUE), ",\n",
        "    \"records_with_stretch_applied\": ", sum(result$OriginalPrecipitation > 0 & result$DailyStretchRatio != 1.0, na.rm = TRUE), ",\n",
        "    \"records_with_missing_ratios\": ", missing_ratios, ",\n",
        "    \"date_range_start\": \"", as.character(min(as.Date(merged_data$date_only), na.rm = TRUE)), "\",\n",
        "    \"date_range_end\": \"", as.character(max(as.Date(merged_data$date_only), na.rm = TRUE)), "\",\n",
        "    \"total_original_precipitation\": ", round(sum(result$OriginalPrecipitation, na.rm = TRUE), 6), ",\n",
        "    \"total_stretched_precipitation\": ", round(sum(result$StretchedPrecipitation, na.rm = TRUE), 6), ",\n",
        "    \"average_stretch_ratio_nonzero\": ", round(mean(result$DailyStretchRatio[result$OriginalPrecipitation > 0], na.rm = TRUE), 6), ",\n",
        "    \"mass_balance_ratio\": ", round(sum(result$StretchedPrecipitation, na.rm = TRUE) / sum(result$OriginalPrecipitation, na.rm = TRUE), 6), ",\n",
        "    \"sign_correction_applied\": ", tolower(as.character(daily_precip_available && "DailyPrecipitation" %in% names(merged_data))), "\n",
        "  },\n",
        "  \"generation_info\": {\n",
        "    \"date_generated\": \"", Sys.Date(), "\",\n",
        "    \"timestamp_generated\": \"", Sys.time(), "\",\n",
        "    \"r_version\": \"", R.version.string, "\",\n",
        "    \"function_name\": \"apply_stretch_ratios\"\n",
        "  }\n",
        "}"
      )
      writeLines(json_content, output_json_file)
      cat("Metadata saved to:", output_json_file, " (manual JSON format)\n")
    }
  }, error = function(e) {
    warning(paste("Could not write JSON metadata file:", e$message))
  })
  
  # Print summary statistics
  cat("\n=== SUMMARY ===\n")
  cat("Sub-daily input records:", nrow(subdaily_data), "\n")
  cat("Daily stretch ratio records:", nrow(daily_data), "\n")
  cat("Output records:", nrow(result), "\n")
  cat("Records with non-zero precipitation:", sum(result$OriginalPrecipitation > 0, na.rm = TRUE), "\n")
  cat("Records with stretch ratios applied:", sum(result$OriginalPrecipitation > 0 & result$DailyStretchRatio != 1.0, na.rm = TRUE), "\n")
  cat("Records with missing stretch ratios (set to 1.0):", missing_ratios, "\n")
  cat("Sign correction applied:", if (daily_precip_available && "DailyPrecipitation" %in% names(merged_data)) "Yes" else "No", "\n")
  cat("Threshold value used in daily aggregation:", if (!is.na(threshold_value)) threshold_value else "not available", "\n")
  cat("Date range:", as.character(min(as.Date(merged_data$date_only), na.rm = TRUE)), 
      "to", as.character(max(as.Date(merged_data$date_only), na.rm = TRUE)), "\n")
  cat("Total original precipitation:", round(sum(result$OriginalPrecipitation, na.rm = TRUE), 4), "\n")
  cat("Total stretched precipitation:", round(sum(result$StretchedPrecipitation, na.rm = TRUE), 4), "\n")
  cat("Mass balance ratio (stretched/original):", round(sum(result$StretchedPrecipitation, na.rm = TRUE) / sum(result$OriginalPrecipitation, na.rm = TRUE), 4), "\n")
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