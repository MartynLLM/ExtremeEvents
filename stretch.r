#' Calculate Precipitation Percentiles and Stretched Precipitation
#'
#' This function processes precipitation data, calculates percentiles,
#' and creates a "stretched" precipitation series based on specified parameters.
#'
#' @param input_file Path to the input CSV file (default: "daily_precipitation.csv")
#' @param output_file Path to the output CSV file (default: "precipitation_stretched.csv")
#' @param threshold_percentile Percentile threshold for stretching (default: 95)
#' @param stretch_amount Amount to stretch by in percentage (default: 1.0, representing 100%)
#' @param scaling_factor Scaling factor for the stretching formula (default: 1.0)
#' @return A data frame with Date, Precipitation, Percentile, StretchedPrecipitation, and StretchRatio columns
#'
calculate_stretched_precipitation <- function(input_file = "daily_precipitation.csv", 
                                             output_file = "precipitation_stretched.csv",
                                             threshold_percentile = 95,
                                             stretch_amount = 0.0,
                                             scaling_factor = 1.0) {
  
  # Read the input CSV file
  data <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # Ensure we have the expected columns
  if(!all(c("Date", "Precipitation") %in% names(data))) {
    stop("Input file must contain 'Date' and 'Precipitation' columns")
  }
  
  # Make a copy of all data (including zero precipitation days)
  result <- data
  
  # Initialize the Percentile, StretchedPrecipitation, and StretchRatio columns
  result$Percentile <- NA
  result$StretchedPrecipitation <- result$Precipitation  # Initialize with original values
  result$StretchRatio <- NA  # Initialize StretchRatio column
  
  # Extract only the non-zero precipitation values for percentile calculation
  non_zero_data <- data[data$Precipitation > 0, ]
  
  # Calculate ranks using R's rank function with average for ties
  if(nrow(non_zero_data) > 0) {
    ranks <- rank(non_zero_data$Precipitation, ties.method = "average")
    
    # Convert ranks to percentiles (0 to 100 scale)
    n <- length(ranks)
    if(n > 1) {
      percentiles <- (ranks - 1) / (n - 1) * 100
    } else {
      # If there's only one non-zero value, assign it percentile 0
      percentiles <- 0
    }
    
    # Create a mapping table of precipitation values to percentiles
    percentile_map <- data.frame(
      Precipitation = non_zero_data$Precipitation,
      Percentile = percentiles
    )
    
    # Remove duplicates to get a unique mapping
    percentile_map <- aggregate(Percentile ~ Precipitation, data = percentile_map, FUN = mean)
    
    # Apply the mapping to all non-zero precipitation values in the full dataset
    for(i in 1:nrow(percentile_map)) {
      precip_value <- percentile_map$Precipitation[i]
      percentile_value <- percentile_map$Percentile[i]
      result$Percentile[result$Precipitation == precip_value] <- percentile_value
    }
    
    # Calculate stretched precipitation and stretch ratio
    for(i in 1:nrow(result)) {
      if(result$Precipitation[i] > 0) {  # Only calculate for non-zero precipitation
        percentile <- result$Percentile[i]
        precip <- result$Precipitation[i]
        
        if(percentile >= threshold_percentile) {
          # Formula for percentiles >= threshold
          stretched <- (1 + (((percentile - threshold_percentile) / 
                            (100 - threshold_percentile))^scaling_factor) * stretch_amount) * precip
        } else {
          # Formula for percentiles < threshold
          stretched <- (1 - sign(stretch_amount) * (((threshold_percentile - percentile) / 
                                threshold_percentile)^scaling_factor)) * precip
        }
        
        result$StretchedPrecipitation[i] <- stretched
        
        # Calculate stretch ratio (stretched / original)
        result$StretchRatio[i] <- stretched / precip
        
      } else {
        # Zero precipitation remains zero, and stretch ratio is NA for zero days
        result$StretchedPrecipitation[i] <- 0
        result$StretchRatio[i] <- NA
      }
    }
  }
  
  # Sort by date for the final output
  result <- result[order(result$Date), ]
  
  # Write the complete result to the output file
  write.csv(result, output_file, row.names = FALSE)
  
  # Print summary
  cat("Processing complete!\n")
  cat("Total days:", nrow(result), "\n")
  cat("Days with precipitation > 0:", sum(result$Precipitation > 0), "\n")
  cat("Days with precipitation = 0:", sum(result$Precipitation == 0), "\n")
  cat("Threshold percentile:", threshold_percentile, "\n")
  cat("Stretch amount:", stretch_amount * 100, "%\n")
  cat("Scaling factor:", scaling_factor, "\n")
  cat("Mean stretch ratio for non-zero days:", round(mean(result$StretchRatio, na.rm = TRUE), 3), "\n")
  cat("Output saved to:", output_file, "\n")
  
  return(result)
}

# Example usage:
# Default parameters (threshold=95%, stretch=100%, scaling=1.0)
# result <- calculate_stretched_precipitation()
#
# Custom parameters:
# result <- calculate_stretched_precipitation(
#   threshold_percentile = 90,
#   stretch_amount = 1.5,  # 150%
#   scaling_factor = 0.8
# )