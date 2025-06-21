#' Debug Version: Apply Daily Stretch Ratios to Sub-daily Precipitation Data
#'
#' This debug version includes detailed diagnostics to identify the merge issue

apply_stretch_ratios_debug <- function(daily_file = "precipitation_stretched.csv",
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
  
  # DEBUG: Check daily data structure
  cat("\n=== DAILY DATA DIAGNOSTICS ===\n")
  cat("Daily data columns:", paste(names(daily_data), collapse = ", "), "\n")
  cat("Daily data dimensions:", nrow(daily_data), "rows,", ncol(daily_data), "columns\n")
  cat("First few rows of daily data:\n")
  print(head(daily_data, 3))
  
  # Check if required columns exist in daily file
  if (!all(c("Date", "StretchRatio") %in% names(daily_data))) {
    cat("Available columns:", paste(names(daily_data), collapse = ", "), "\n")
    stop("Daily file must contain 'Date' and 'StretchRatio' columns")
  }
  
  # Convert Date column to Date format in daily data
  cat("Original Date column sample:", head(daily_data$Date, 3), "\n")
  daily_data$Date <- as.Date(daily_data$Date)
  cat("Converted Date column sample:", head(daily_data$Date, 3), "\n")
  cat("Date range in daily data:", min(daily_data$Date, na.rm = TRUE), "to", max(daily_data$Date, na.rm = TRUE), "\n")
  
  # Read the sub-daily precipitation file
  cat("\nReading sub-daily precipitation from:", subdaily_file, "\n")
  subdaily_data <- read.csv(subdaily_file, stringsAsFactors = FALSE)
  
  # DEBUG: Check subdaily data structure
  cat("\n=== SUB-DAILY DATA DIAGNOSTICS ===\n")
  cat("Sub-daily data columns:", paste(names(subdaily_data), collapse = ", "), "\n")
  cat("Sub-daily data dimensions:", nrow(subdaily_data), "rows,", ncol(subdaily_data), "columns\n")
  cat("First few rows of sub-daily data:\n")
  print(head(subdaily_data, 3))
  
  # Check if required columns exist in sub-daily file
  if (!all(c("date", "Precipitation") %in% names(subdaily_data))) {
    cat("Available columns:", paste(names(subdaily_data), collapse = ", "), "\n")
    stop("Sub-daily file must contain 'date' and 'Precipitation' columns")
  }
  
  # Convert datetime column to POSIXct (handles YYYY-MM-DD HH24:MI format)
  cat("\nConverting datetime format...\n")
  cat("Original date column sample:", head(subdaily_data$date, 3), "\n")
  subdaily_data$datetime <- as.POSIXct(subdaily_data$date, format = "%Y-%m-%d %H:%M")
  
  # Check if date conversion was successful
  failed_conversions <- sum(is.na(subdaily_data$datetime))
  if (failed_conversions > 0) {
    warning(paste("Failed to parse", failed_conversions, "dates. Check date format."))
    cat("Failed date examples:", head(subdaily_data$date[is.na(subdaily_data$datetime)], 5), "\n")
  }
  
  # Extract date only (YYYY-MM-DD format) for matching
  subdaily_data$date_only <- as.Date(subdaily_data$datetime)
  cat("Converted datetime sample:", head(subdaily_data$datetime, 3), "\n")
  cat("Extracted date_only sample:", head(subdaily_data$date_only, 3), "\n")
  
  # Remove rows with invalid dates
  original_rows <- nrow(subdaily_data)
  subdaily_data <- subdaily_data[!is.na(subdaily_data$date_only), ]
  cat("Removed", original_rows - nrow(subdaily_data), "rows with invalid dates\n")
  cat("Date range in sub-daily data:", min(subdaily_data$date_only, na.rm = TRUE), "to", max(subdaily_data$date_only, na.rm = TRUE), "\n")
  
  # DEBUG: Check for date overlap
  cat("\n=== DATE OVERLAP DIAGNOSTICS ===\n")
  unique_daily_dates <- unique(daily_data$Date)
  unique_subdaily_dates <- unique(subdaily_data$date_only)
  cat("Unique dates in daily data:", length(unique_daily_dates), "\n")
  cat("Unique dates in sub-daily data:", length(unique_subdaily_dates), "\n")
  
  overlapping_dates <- intersect(unique_daily_dates, unique_subdaily_dates)
  cat("Overlapping dates:", length(overlapping_dates), "\n")
  
  if (length(overlapping_dates) == 0) {
    cat("ERROR: NO OVERLAPPING DATES FOUND!\n")
    cat("Daily data date range:", min(unique_daily_dates, na.rm = TRUE), "to", max(unique_daily_dates, na.rm = TRUE), "\n")
    cat("Sub-daily data date range:", min(unique_subdaily_dates, na.rm = TRUE), "to", max(unique_subdaily_dates, na.rm = TRUE), "\n")
    stop("No overlapping dates between daily and sub-daily data. Cannot proceed with merge.")
  } else {
    cat("First few overlapping dates:", head(overlapping_dates, 5), "\n")
    cat("Last few overlapping dates:", tail(overlapping_dates, 5), "\n")
  }
  
  # Merge sub-daily data with daily stretch ratios
  cat("\nMerging data and applying stretch ratios...\n")
  cat("Before merge - sub-daily rows:", nrow(subdaily_data), "\n")
  cat("Before merge - daily rows:", nrow(daily_data), "\n")
  
  merged_data <- merge(subdaily_data, daily_data, by.x = "date_only", by.y = "Date", all.x = TRUE)
  
  cat("After merge - merged rows:", nrow(merged_data), "\n")
  cat("Merged data columns:", paste(names(merged_data), collapse = ", "), "\n")
  
  # DEBUG: Check merge results
  cat("\n=== MERGE RESULTS DIAGNOSTICS ===\n")
  cat("Merged data dimensions:", nrow(merged_data), "rows,", ncol(merged_data), "columns\n")
  
  # Check for missing values in key columns
  missing_date <- sum(is.na(merged_data$date))
  missing_precip <- sum(is.na(merged_data$Precipitation))
  missing_stretch <- sum(is.na(merged_data$StretchRatio))
  
  cat("Missing values in 'date' column:", missing_date, "\n")
  cat("Missing values in 'Precipitation' column:", missing_precip, "\n")
  cat("Missing values in 'StretchRatio' column:", missing_stretch, "\n")
  
  # Show sample of merged data
  cat("Sample of merged data:\n")
  print(head(merged_data[, c("date", "Precipitation", "StretchRatio")], 3))
  
  # Check column lengths before creating data.frame
  cat("\n=== COLUMN LENGTH CHECK ===\n")
  cat("Length of merged_data$date:", length(merged_data$date), "\n")
  cat("Length of merged_data$Precipitation:", length(merged_data$Precipitation), "\n")
  
  if ("StretchRatio" %in% names(merged_data)) {
    cat("Length of merged_data$StretchRatio:", length(merged_data$StretchRatio), "\n")
  } else {
    cat("ERROR: StretchRatio column not found in merged data!\n")
    cat("Available columns:", paste(names(merged_data), collapse = ", "), "\n")
    return(NULL)
  }
  
  # If we get here, try to create the result data frame
  cat("\nAttempting to create result data frame...\n")
  
  # Initialize stretched precipitation column
  merged_data$StretchedPrecipitation <- merged_data$Precipitation
  
  # Apply stretch ratios only to non-zero precipitation values
  non_zero_mask <- merged_data$Precipitation > 0 & !is.na(merged_data$StretchRatio)
  cat("Records with non-zero precipitation and valid stretch ratio:", sum(non_zero_mask), "\n")
  
  merged_data$StretchedPrecipitation[non_zero_mask] <- 
    merged_data$Precipitation[non_zero_mask] * merged_data$StretchRatio[non_zero_mask]
  
  # Create final output dataframe with desired columns
  result <- data.frame(
    DateTime = merged_data$date,
    OriginalPrecipitation = merged_data$Precipitation,
    StretchedPrecipitation = merged_data$StretchedPrecipitation,
    DailyStretchRatio = merged_data$StretchRatio
  )
  
  cat("Successfully created result data frame with", nrow(result), "rows\n")
  
  return(result)
}

# Run the debug version
cat("Running debug version of apply_stretch_ratios...\n")
debug_result <- apply_stretch_ratios_debug()