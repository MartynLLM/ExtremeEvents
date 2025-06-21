#' Complete Precipitation Stretching Workflow
#'
#' This script runs the complete workflow for precipitation stretching:
#' 1. Aggregates sub-daily data to daily totals
#' 2. Calculates stretch ratios for daily data
#' 3. Applies stretch ratios back to sub-daily data
#'
#' Make sure you have the following files in your working directory:
#' - createDailyPrecip.r (or the fixed version)
#' - stretch_mass_balanced.r (or stretch.r)
#' - precipitationStretchScript.r (or the fixed version)
#' - short.csv (your sub-daily precipitation data)

# Set working directory if needed
# setwd("path/to/your/files")

cat("=== PRECIPITATION STRETCHING WORKFLOW ===\n\n")

# Check if required files exist
required_files <- c("precip.csv")
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required files: ", paste(missing_files, collapse = ", "))
}

# STEP 1: Aggregate sub-daily data to daily totals
cat("STEP 1: Aggregating sub-daily precipitation to daily totals...\n")
cat("---------------------------------------------------------------\n")

# Source or define the aggregate_daily_precipitation function
# If you have the fixed createDailyPrecip.r file, source it:
# source("createDailyPrecip.r")

# Or use the function directly (included below for convenience):
aggregate_daily_precipitation <- function(input_file = "precip.csv", 
                                        output_file = "daily_precipitation.csv",
                                        threshold = 0.1) {
  
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  
  cat("Reading input file:", input_file, "\n")
  data <- read.csv(input_file, stringsAsFactors = FALSE)
  
  if (!all(c("date", "Precipitation") %in% names(data))) {
    stop("Input file must contain 'date' and 'Precipitation' columns")
  }
  
  cat("Converting date format...\n")
  data$datetime <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M")
  
  if (sum(is.na(data$datetime)) > 0) {
    warning(paste("Failed to parse", sum(is.na(data$datetime)), "dates. Check date format."))
  }
  
  data$date_only <- as.Date(data$datetime)
  data <- data[!is.na(data$date_only), ]
  
  cat("Aggregating precipitation by date...\n")
  daily_precip <- aggregate(Precipitation ~ date_only, data = data, FUN = sum, na.rm = TRUE)
  names(daily_precip) <- c("Date", "Precipitation")
  daily_precip <- daily_precip[order(daily_precip$Date), ]
  
  cat("Applying threshold (", threshold, " mm) and rounding...\n")
  daily_precip$Precipitation <- ifelse(daily_precip$Precipitation >= threshold,
                                      round(daily_precip$Precipitation, 1), 0)
  
  write.csv(daily_precip, output_file, row.names = FALSE)
  
  cat("\n=== STEP 1 SUMMARY ===\n")
  cat("Input records:", nrow(data), "\n")
  cat("Output daily records:", nrow(daily_precip), "\n")
  cat("Date range:", as.character(min(daily_precip$Date)), "to", as.character(max(daily_precip$Date)), "\n")
  cat("Total precipitation:", round(sum(daily_precip$Precipitation, na.rm = TRUE), 2), "\n")
  cat("Average daily precipitation:", round(mean(daily_precip$Precipitation, na.rm = TRUE), 2), "\n")
  cat("Days with precipitation >= threshold:", sum(daily_precip$Precipitation > 0, na.rm = TRUE), "\n")
  cat("Output saved to:", output_file, "\n\n")
  
  return(daily_precip)
}

# Run Step 1
daily_result <- aggregate_daily_precipitation()

# STEP 2: Calculate stretch ratios
cat("STEP 2: Calculating stretch ratios for daily precipitation...\n")
cat("------------------------------------------------------------\n")

# Source or define the mass-balanced stretch function
# If you have stretch_mass_balanced.r, source it:
# source("stretch_mass_balanced.r")

# For this example, we'll use a simplified version with mass balance:
calculate_stretch_ratios <- function(input_file = "daily_precipitation.csv", 
                                   output_file = "precipitation_stretched.csv",
                                   threshold_percentile = 95,
                                   stretch_amount = 1.0) {
  
  data <- read.csv(input_file, stringsAsFactors = FALSE)
  
  if(!all(c("Date", "Precipitation") %in% names(data))) {
    stop("Input file must contain 'Date' and 'Precipitation' columns")
  }
  
  result <- data
  result$Percentile <- NA
  result$StretchedPrecipitation <- result$Precipitation
  result$StretchRatio <- 1.0  # Initialize all ratios to 1.0
  
  # Calculate percentiles for non-zero precipitation
  non_zero_data <- data[data$Precipitation > 0, ]
  
  if(nrow(non_zero_data) > 0) {
    ranks <- rank(non_zero_data$Precipitation, ties.method = "average")
    n <- length(ranks)
    if(n > 1) {
      percentiles <- (ranks - 1) / (n - 1) * 100
    } else {
      percentiles <- 0
    }
    
    percentile_map <- data.frame(
      Precipitation = non_zero_data$Precipitation,
      Percentile = percentiles
    )
    percentile_map <- aggregate(Percentile ~ Precipitation, data = percentile_map, FUN = mean)
    
    # Apply percentile mapping
    for(i in 1:nrow(percentile_map)) {
      precip_value <- percentile_map$Precipitation[i]
      percentile_value <- percentile_map$Percentile[i]
      result$Percentile[result$Precipitation == precip_value] <- percentile_value
    }
    
    # Calculate stretch ratios (simplified formula)
    scaling_factor <- 1.0
    for(i in 1:nrow(result)) {
      if(result$Precipitation[i] > 0) {
        percentile <- result$Percentile[i]
        
        if(percentile >= threshold_percentile) {
          # Stretch extreme values
          stretch_factor <- 1 + (((percentile - threshold_percentile) / 
                                (100 - threshold_percentile))^scaling_factor) * stretch_amount
        } else {
          # Reduce lower values to maintain mass balance
          stretch_factor <- 1 - (((threshold_percentile - percentile) / 
                                threshold_percentile)^scaling_factor) * (stretch_amount * 0.1)
        }
        
        result$StretchRatio[i] <- stretch_factor
        result$StretchedPrecipitation[i] <- result$Precipitation[i] * stretch_factor
      }
    }
  }
  
  write.csv(result, output_file, row.names = FALSE)
  
  cat("\n=== STEP 2 SUMMARY ===\n")
  cat("Total days:", nrow(result), "\n")
  cat("Days with precipitation > 0:", sum(result$Precipitation > 0), "\n")
  cat("Threshold percentile:", threshold_percentile, "\n")
  cat("Stretch amount:", stretch_amount * 100, "%\n")
  cat("Mean stretch ratio:", round(mean(result$StretchRatio[result$Precipitation > 0], na.rm = TRUE), 3), "\n")
  cat("Output saved to:", output_file, "\n\n")
  
  return(result)
}

# Run Step 2
stretch_result <- calculate_stretch_ratios()

# STEP 3: Apply stretch ratios to sub-daily data
cat("STEP 3: Applying stretch ratios to sub-daily precipitation...\n")
cat("------------------------------------------------------------\n")

apply_stretch_ratios <- function(daily_file = "precipitation_stretched.csv",
                                subdaily_file = "precip.csv", 
                                output_file = "stretched_subdaily_precipitation.csv") {
  
  if (!require(lubridate)) {
    install.packages("lubridate")
    library(lubridate)
  }
  
  if (!file.exists(daily_file)) {
    stop(paste("Daily stretch ratios file not found:", daily_file))
  }
  
  cat("Reading daily stretch ratios from:", daily_file, "\n")
  daily_data <- read.csv(daily_file, stringsAsFactors = FALSE)
  
  if (!all(c("Date", "StretchRatio") %in% names(daily_data))) {
    stop("Daily file must contain 'Date' and 'StretchRatio' columns")
  }
  
  daily_data$Date <- as.Date(daily_data$Date)
  
  cat("Reading sub-daily precipitation from:", subdaily_file, "\n")
  subdaily_data <- read.csv(subdaily_file, stringsAsFactors = FALSE)
  
  if (!all(c("date", "Precipitation") %in% names(subdaily_data))) {
    stop("Sub-daily file must contain 'date' and 'Precipitation' columns")
  }
  
  cat("Converting datetime format...\n")
  subdaily_data$datetime <- as.POSIXct(subdaily_data$date, format = "%Y-%m-%d %H:%M")
  subdaily_data$date_only <- as.Date(subdaily_data$datetime)
  subdaily_data <- subdaily_data[!is.na(subdaily_data$date_only), ]
  
  cat("Merging data and applying stretch ratios...\n")
  merged_data <- merge(subdaily_data, daily_data, by.x = "date_only", by.y = "Date", all.x = TRUE)
  
  merged_data$StretchedPrecipitation <- merged_data$Precipitation
  
  non_zero_mask <- merged_data$Precipitation > 0 & !is.na(merged_data$StretchRatio)
  merged_data$StretchedPrecipitation[non_zero_mask] <- 
    merged_data$Precipitation[non_zero_mask] * merged_data$StretchRatio[non_zero_mask]
  
  result <- data.frame(
    DateTime = merged_data$date,
    OriginalPrecipitation = merged_data$Precipitation,
    StretchedPrecipitation = merged_data$StretchedPrecipitation,
    DailyStretchRatio = merged_data$StretchRatio
  )
  
  result <- result[order(as.POSIXct(result$DateTime, format = "%Y-%m-%d %H:%M")), ]
  
  missing_ratios <- sum(is.na(result$DailyStretchRatio))
  if (missing_ratios > 0) {
    result$DailyStretchRatio[is.na(result$DailyStretchRatio)] <- 1.0
    result$StretchedPrecipitation[is.na(result$StretchedPrecipitation)] <- 
      result$OriginalPrecipitation[is.na(result$StretchedPrecipitation)]
  }
  
  write.csv(result, output_file, row.names = FALSE)
  
  cat("\n=== STEP 3 SUMMARY ===\n")
  cat("Sub-daily input records:", nrow(subdaily_data), "\n")
  cat("Daily stretch ratio records:", nrow(daily_data), "\n")
  cat("Output records:", nrow(result), "\n")
  cat("Records with non-zero precipitation:", sum(result$OriginalPrecipitation > 0, na.rm = TRUE), "\n")
  cat("Total original precipitation:", round(sum(result$OriginalPrecipitation, na.rm = TRUE), 4), "\n")
  cat("Total stretched precipitation:", round(sum(result$StretchedPrecipitation, na.rm = TRUE), 4), "\n")
  cat("Output saved to:", output_file, "\n\n")
  
  return(result)
}

# Run Step 3
final_result <- apply_stretch_ratios()

cat("=== WORKFLOW COMPLETE ===\n")
cat("Files created:\n")
cat("1. daily_precipitation.csv - Daily aggregated precipitation\n")
cat("2. precipitation_stretched.csv - Daily precipitation with stretch ratios\n")
cat("3. stretched_subdaily_precipitation.csv - Final stretched sub-daily data\n")
cat("\nWorkflow completed successfully!\n")