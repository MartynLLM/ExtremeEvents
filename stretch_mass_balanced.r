#' Calculate Precipitation Percentiles and Mass-Balanced Stretched Precipitation
#'
#' This function processes precipitation data, calculates percentiles,
#' and creates a "stretched" precipitation series that maintains mass balance
#' by automatically adjusting the scaling factor so that the sum of original
#' precipitation equals the sum of stretched precipitation.
#'
#' @param input_file Path to the input CSV file (default: "daily_precipitation.csv")
#' @param output_file Path to the output CSV file (default: "precipitation_stretched.csv")
#' @param threshold_percentile Percentile threshold for stretching (default: 95)
#' @param stretch_amount Amount to stretch by in percentage (default: 1.0, representing 100%)
#' @param initial_scaling_factor Initial guess for scaling factor (default: 1.0)
#' @param tolerance Tolerance for mass balance convergence (default: 0.001)
#' @param max_iterations Maximum iterations for scaling factor optimization (default: 100)
#' @return A data frame with Date, Precipitation, Percentile, StretchedPrecipitation, and StretchRatio columns
#'
calculate_mass_balanced_stretched_precipitation <- function(input_file = "daily_precipitation.csv", 
                                                          output_file = "precipitation_stretched.csv",
                                                          threshold_percentile = 95,
                                                          stretch_amount = 1.0,
                                                          initial_scaling_factor = 1.0,
                                                          tolerance = 0.001,
                                                          max_iterations = 100) {
  
  # Read the input CSV file
  data <- read.csv(input_file, stringsAsFactors = FALSE)
  
  # Ensure we have the expected columns
  if(!all(c("Date", "Precipitation") %in% names(data))) {
    stop("Input file must contain 'Date' and 'Precipitation' columns")
  }
  
  # Calculate the target sum (original precipitation sum)
  target_sum <- sum(data$Precipitation, na.rm = TRUE)
  
  # If stretch_amount is 0, no stretching needed - return original data
  if(stretch_amount == 0) {
    result <- data
    result$Percentile <- NA
    result$StretchedPrecipitation <- result$Precipitation
    result$StretchRatio <- 1
    result$Percentile[result$Precipitation == 0] <- NA
    
    # Calculate percentiles for non-zero values
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
      
      for(i in 1:nrow(percentile_map)) {
        precip_value <- percentile_map$Precipitation[i]
        percentile_value <- percentile_map$Percentile[i]
        result$Percentile[result$Precipitation == precip_value] <- percentile_value
      }
    }
    
    write.csv(result, output_file, row.names = FALSE)
    cat("No stretching applied (stretch_amount = 0). Original data preserved.\n")
    
    # Create JSON metadata file for no-stretch case
    json_file <- sub("\\.csv$", ".json", output_file)
    metadata <- list(
      parameters = list(
        input_file = input_file,
        output_file = output_file,
        threshold_percentile = threshold_percentile,
        stretch_amount = stretch_amount,
        initial_scaling_factor = initial_scaling_factor,
        tolerance = tolerance,
        max_iterations = max_iterations
      ),
      results = list(
        optimal_scaling_factor = 1.0,
        original_precipitation_sum = target_sum,
        stretched_precipitation_sum = target_sum,
        mass_balance_error_percent = 0.0,
        total_days = nrow(result),
        days_with_precipitation = sum(result$Precipitation > 0),
        days_without_precipitation = sum(result$Precipitation == 0),
        mean_stretch_ratio = 1.0
      ),
      generation_info = list(
        date_generated = Sys.Date(),
        timestamp_generated = Sys.time(),
        r_version = R.version.string
      )
    )
    
    # Write JSON metadata file
    if(require(jsonlite, quietly = TRUE)) {
      write_json(metadata, json_file, pretty = TRUE, auto_unbox = TRUE)
      cat("Metadata saved to:", json_file, "\n")
    } else {
      # Fallback: write JSON manually if jsonlite is not available
      json_content <- paste0(
        "{\n",
        "  \"parameters\": {\n",
        "    \"input_file\": \"", input_file, "\",\n",
        "    \"output_file\": \"", output_file, "\",\n",
        "    \"threshold_percentile\": ", threshold_percentile, ",\n",
        "    \"stretch_amount\": ", stretch_amount, ",\n",
        "    \"initial_scaling_factor\": ", initial_scaling_factor, ",\n",
        "    \"tolerance\": ", tolerance, ",\n",
        "    \"max_iterations\": ", max_iterations, "\n",
        "  },\n",
        "  \"results\": {\n",
        "    \"optimal_scaling_factor\": 1.0,\n",
        "    \"original_precipitation_sum\": ", target_sum, ",\n",
        "    \"stretched_precipitation_sum\": ", target_sum, ",\n",
        "    \"mass_balance_error_percent\": 0.0,\n",
        "    \"total_days\": ", nrow(result), ",\n",
        "    \"days_with_precipitation\": ", sum(result$Precipitation > 0), ",\n",
        "    \"days_without_precipitation\": ", sum(result$Precipitation == 0), ",\n",
        "    \"mean_stretch_ratio\": 1.0\n",
        "  },\n",
        "  \"generation_info\": {\n",
        "    \"date_generated\": \"", Sys.Date(), "\",\n",
        "    \"timestamp_generated\": \"", Sys.time(), "\",\n",
        "    \"r_version\": \"", R.version.string, "\"\n",
        "  }\n",
        "}"
      )
      writeLines(json_content, json_file)
      cat("Metadata saved to:", json_file, " (manual JSON format)\n")
    }
    
    return(result)
  }
  
  # Helper function to calculate stretched precipitation for a given scaling factor
  calculate_stretch <- function(scaling_factor) {
    result_temp <- data
    result_temp$Percentile <- NA
    result_temp$StretchedPrecipitation <- result_temp$Precipitation
    result_temp$StretchRatio <- NA
    
    # Extract only the non-zero precipitation values for percentile calculation
    non_zero_data <- data[data$Precipitation > 0, ]
    
    if(nrow(non_zero_data) > 0) {
      ranks <- rank(non_zero_data$Precipitation, ties.method = "average")
      n <- length(ranks)
      if(n > 1) {
        percentiles <- (ranks - 1) / (n - 1) * 100
      } else {
        percentiles <- 0
      }
      
      # Create a mapping table of precipitation values to percentiles
      percentile_map <- data.frame(
        Precipitation = non_zero_data$Precipitation,
        Percentile = percentiles
      )
      percentile_map <- aggregate(Percentile ~ Precipitation, data = percentile_map, FUN = mean)
      
      # Apply the mapping to all non-zero precipitation values in the full dataset
      for(i in 1:nrow(percentile_map)) {
        precip_value <- percentile_map$Precipitation[i]
        percentile_value <- percentile_map$Percentile[i]
        result_temp$Percentile[result_temp$Precipitation == precip_value] <- percentile_value
      }
      
      # Calculate stretched precipitation
      for(i in 1:nrow(result_temp)) {
        if(result_temp$Precipitation[i] > 0) {
          percentile <- result_temp$Percentile[i]
          precip <- result_temp$Precipitation[i]
          
          if(percentile >= threshold_percentile) {
            # Formula for percentiles >= threshold
            stretched <- (1 + (((percentile - threshold_percentile) / 
                              (100 - threshold_percentile))^scaling_factor) * stretch_amount) * precip
          } else {
            # Formula for percentiles < threshold
            stretched <- (1 - sign(stretch_amount) * (((threshold_percentile - percentile) / 
                                  threshold_percentile)^scaling_factor)) * precip
          }
          
          result_temp$StretchedPrecipitation[i] <- stretched
          result_temp$StretchRatio[i] <- stretched / precip
        } else {
          result_temp$StretchedPrecipitation[i] <- 0
          result_temp$StretchRatio[i] <- NA
        }
      }
    }
    
    return(result_temp)
  }
  
  # Helper function to calculate the difference between target and actual sum
  calculate_sum_difference <- function(scaling_factor) {
    result_temp <- calculate_stretch(scaling_factor)
    actual_sum <- sum(result_temp$StretchedPrecipitation, na.rm = TRUE)
    return(actual_sum - target_sum)
  }
  
  # Use bisection method to find the scaling factor that balances mass
  cat("Optimizing scaling factor for mass balance...\n")
  
  # Start with a reasonable range for scaling factor
  lower_bound <- 0.1
  upper_bound <- 10.0
  
  # Check if the solution is within our bounds
  lower_diff <- calculate_sum_difference(lower_bound)
  upper_diff <- calculate_sum_difference(upper_bound)
  
  # If both bounds have the same sign, expand the search range
  if(sign(lower_diff) == sign(upper_diff)) {
    if(lower_diff > 0) {
      # Both positive, need smaller scaling factor
      upper_bound <- lower_bound
      lower_bound <- 0.01
    } else {
      # Both negative, need larger scaling factor
      lower_bound <- upper_bound
      upper_bound <- 100.0
    }
  }
  
  # Bisection method
  optimal_scaling_factor <- initial_scaling_factor
  for(iteration in 1:max_iterations) {
    mid_point <- (lower_bound + upper_bound) / 2
    mid_diff <- calculate_sum_difference(mid_point)
    
    if(abs(mid_diff) < tolerance) {
      optimal_scaling_factor <- mid_point
      cat("Converged after", iteration, "iterations\n")
      break
    }
    
    if(sign(mid_diff) == sign(calculate_sum_difference(lower_bound))) {
      lower_bound <- mid_point
    } else {
      upper_bound <- mid_point
    }
    
    optimal_scaling_factor <- mid_point
    
    if(iteration == max_iterations) {
      cat("Warning: Maximum iterations reached. May not have fully converged.\n")
    }
  }
  
  # Calculate final result with optimal scaling factor
  result <- calculate_stretch(optimal_scaling_factor)
  
  # Sort by date for the final output
  result <- result[order(result$Date), ]
  
  # Calculate final sums for verification
  original_sum <- sum(result$Precipitation, na.rm = TRUE)
  stretched_sum <- sum(result$StretchedPrecipitation, na.rm = TRUE)
  mass_balance_error <- abs(stretched_sum - original_sum) / original_sum * 100
  
  # Write the complete result to the output file
  write.csv(result, output_file, row.names = FALSE)
  
  # Create JSON metadata file
  json_file <- sub("\\.csv$", ".json", output_file)
  
  # Prepare metadata
  metadata <- list(
    parameters = list(
      input_file = input_file,
      output_file = output_file,
      threshold_percentile = threshold_percentile,
      stretch_amount = stretch_amount,
      initial_scaling_factor = initial_scaling_factor,
      tolerance = tolerance,
      max_iterations = max_iterations
    ),
    results = list(
      optimal_scaling_factor = optimal_scaling_factor,
      original_precipitation_sum = original_sum,
      stretched_precipitation_sum = stretched_sum,
      mass_balance_error_percent = mass_balance_error,
      total_days = nrow(result),
      days_with_precipitation = sum(result$Precipitation > 0),
      days_without_precipitation = sum(result$Precipitation == 0),
      mean_stretch_ratio = round(mean(result$StretchRatio, na.rm = TRUE), 6)
    ),
    generation_info = list(
      date_generated = Sys.Date(),
      timestamp_generated = Sys.time(),
      r_version = R.version.string
    )
  )
  
  # Write JSON metadata file
  if(require(jsonlite, quietly = TRUE)) {
    write_json(metadata, json_file, pretty = TRUE, auto_unbox = TRUE)
    cat("Metadata saved to:", json_file, "\n")
  } else {
    # Fallback: write JSON manually if jsonlite is not available
    json_content <- paste0(
      "{\n",
      "  \"parameters\": {\n",
      "    \"input_file\": \"", input_file, "\",\n",
      "    \"output_file\": \"", output_file, "\",\n",
      "    \"threshold_percentile\": ", threshold_percentile, ",\n",
      "    \"stretch_amount\": ", stretch_amount, ",\n",
      "    \"initial_scaling_factor\": ", initial_scaling_factor, ",\n",
      "    \"tolerance\": ", tolerance, ",\n",
      "    \"max_iterations\": ", max_iterations, "\n",
      "  },\n",
      "  \"results\": {\n",
      "    \"optimal_scaling_factor\": ", optimal_scaling_factor, ",\n",
      "    \"original_precipitation_sum\": ", original_sum, ",\n",
      "    \"stretched_precipitation_sum\": ", stretched_sum, ",\n",
      "    \"mass_balance_error_percent\": ", mass_balance_error, ",\n",
      "    \"total_days\": ", nrow(result), ",\n",
      "    \"days_with_precipitation\": ", sum(result$Precipitation > 0), ",\n",
      "    \"days_without_precipitation\": ", sum(result$Precipitation == 0), ",\n",
      "    \"mean_stretch_ratio\": ", round(mean(result$StretchRatio, na.rm = TRUE), 6), "\n",
      "  },\n",
      "  \"generation_info\": {\n",
      "    \"date_generated\": \"", Sys.Date(), "\",\n",
      "    \"timestamp_generated\": \"", Sys.time(), "\",\n",
      "    \"r_version\": \"", R.version.string, "\"\n",
      "  }\n",
      "}"
    )
    writeLines(json_content, json_file)
    cat("Metadata saved to:", json_file, " (manual JSON format)\n")
  }
  
  # Calculate final sums for verification
  original_sum <- sum(result$Precipitation, na.rm = TRUE)
  stretched_sum <- sum(result$StretchedPrecipitation, na.rm = TRUE)
  mass_balance_error <- abs(stretched_sum - original_sum) / original_sum * 100
  
  # Print summary
  cat("\n=== MASS-BALANCED STRETCHING SUMMARY ===\n")
  cat("Total days:", nrow(result), "\n")
  cat("Days with precipitation > 0:", sum(result$Precipitation > 0), "\n")
  cat("Days with precipitation = 0:", sum(result$Precipitation == 0), "\n")
  cat("Threshold percentile:", threshold_percentile, "\n")
  cat("Stretch amount:", stretch_amount * 100, "%\n")
  cat("Optimal scaling factor:", round(optimal_scaling_factor, 6), "\n")
  cat("Original precipitation sum:", round(original_sum, 4), "\n")
  cat("Stretched precipitation sum:", round(stretched_sum, 4), "\n")
  cat("Mass balance error:", round(mass_balance_error, 6), "%\n")
  cat("Mean stretch ratio for non-zero days:", round(mean(result$StretchRatio, na.rm = TRUE), 3), "\n")
  cat("Output saved to:", output_file, "\n")
  
  return(result)
}

# Example usage:
# Default parameters with mass balance
# result <- calculate_mass_balanced_stretched_precipitation()
#
# Custom parameters with mass balance:
# result <- calculate_mass_balanced_stretched_precipitation(
#   threshold_percentile = 90,
#   stretch_amount = 1.5,  # 150%
#   tolerance = 0.0001     # Higher precision
# )
#
# Run with default parameters
# cat("Starting mass-balanced precipitation stretching...\n")
# result <- calculate_mass_balanced_stretched_precipitation()