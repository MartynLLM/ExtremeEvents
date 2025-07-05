# Precipitation Data Processing Repository Documentation

## Overview

This repository contains R scripts and sample data for processing precipitation data, including aggregation from sub-daily to daily totals and applying percentile-based "stretching" transformations while maintaining mass balance. The tools are designed for climate and hydrological research applications.

## Repository Contents

### Data Files

1. **`seasonal.csv`** (122 rows, 3 columns)
   - Contains seasonal precipitation data
   - Columns: Year (Float), Season (String), Precipitation (Float)

2. **`short.csv`** (34 rows, 2 columns)  
   - Contains sub-daily precipitation data
   - Columns: date (Date), Precipitation (Float)
   - Expected format: YYYY-MM-DD HH:MM

### R Scripts

1. **`createDailyPrecip.r`** - Data aggregation script
2. **`stretch.r`** - Basic precipitation stretching
3. **`stretch_mass_balanced.r`** - Advanced mass-balanced stretching

---

## Script Usage Guide

### 1. Data Aggregation: `createDailyPrecip.r`

**Purpose**: Converts sub-daily precipitation data to daily totals with threshold filtering.

#### Function: `aggregate_daily_precipitation()`

**Parameters:**
- `input_file` (default: "precip.csv") - Path to input CSV file
- `output_file` (default: "daily_precipitation.csv") - Path for output CSV
- `threshold` (default: 0.1) - Minimum precipitation threshold in mm

**Usage Examples:**

```r
# Load the script
source("createDailyPrecip.r")

# Basic usage with defaults
result <- aggregate_daily_precipitation()

# Custom input/output files
result <- aggregate_daily_precipitation(
  input_file = "short.csv",
  output_file = "my_daily_data.csv"
)

# Custom threshold (0.2mm minimum)
result <- aggregate_daily_precipitation(
  input_file = "short.csv", 
  output_file = "daily_precipitation.csv",
  threshold = 0.2
)
```

**Input Requirements:**
- CSV file with columns: `date`, `Precipitation`
- Date format: "YYYY-MM-DD HH:MM"

**Outputs:**
- CSV file with daily precipitation totals
- JSON metadata file with statistics and processing info

---

### 2. Basic Stretching: `stretch.r`

**Purpose**: Applies percentile-based stretching to precipitation data using customizable parameters.

#### Function: `calculate_stretched_precipitation()`

**Parameters:**
- `input_file` (default: "daily_precipitation.csv")
- `output_file` (default: "precipitation_stretched.csv")
- `threshold_percentile` (default: 95) - Percentile threshold for stretching
- `stretch_amount` (default: 0.0) - Stretch amount (1.0 = 100% increase)
- `scaling_factor` (default: 1.0) - Controls stretching curve shape

**Usage Examples:**

```r
# Load the script
source("stretch.r")

# Default parameters (95th percentile, no stretching)
result <- calculate_stretched_precipitation()

# Moderate stretching: 50% increase above 90th percentile
result <- calculate_stretched_precipitation(
  threshold_percentile = 90,
  stretch_amount = 0.5,
  scaling_factor = 1.0
)

# Aggressive stretching: 150% increase above 95th percentile
result <- calculate_stretched_precipitation(
  threshold_percentile = 95,
  stretch_amount = 1.5,
  scaling_factor = 0.8
)
```

**Stretching Formula:**
- **Above threshold**: `(1 + ((percentile - threshold)/(100 - threshold))^scaling_factor * stretch_amount) * precipitation`
- **Below threshold**: `(1 - sign(stretch_amount) * ((threshold - percentile)/threshold)^scaling_factor) * precipitation`

---

### 3. Mass-Balanced Stretching: `stretch_mass_balanced.r`

**Purpose**: Applies stretching while automatically maintaining total precipitation mass balance through optimization.

#### Function: `calculate_mass_balanced_stretched_precipitation()`

**Parameters:**
- `input_file` (default: "daily_precipitation.csv")
- `output_file` (default: "precipitation_stretched.csv") 
- `threshold_percentile` (default: 95)
- `stretch_amount` (default: 1.0)
- `initial_scaling_factor` (default: 1.0) - Starting guess for optimization
- `tolerance` (default: 0.001) - Convergence tolerance for mass balance
- `max_iterations` (default: 100) - Maximum optimization iterations

**Usage Examples:**

```r
# Load the script
source("stretch_mass_balanced.r")

# Default mass-balanced stretching
result <- calculate_mass_balanced_stretched_precipitation()

# Custom parameters with higher precision
result <- calculate_mass_balanced_stretched_precipitation(
  threshold_percentile = 90,
  stretch_amount = 1.5,  # 150% stretching
  tolerance = 0.0001     # Higher precision
)

# Conservative stretching with mass balance
result <- calculate_mass_balanced_stretched_precipitation(
  threshold_percentile = 95,
  stretch_amount = 0.3,  # 30% increase
  tolerance = 0.001
)
```

**Key Features:**
- **Automatic optimization**: Uses bisection method to find optimal scaling factor
- **Mass conservation**: Ensures sum(original) = sum(stretched)
- **Convergence control**: Adjustable tolerance and iteration limits
- **Comprehensive output**: Detailed JSON metadata with optimization results

---

## Complete Workflow Example

Here's a typical end-to-end workflow:

```r
# Step 1: Load all scripts
source("createDailyPrecip.r")
source("stretch_mass_balanced.r")

# Step 2: Convert sub-daily to daily data
daily_data <- aggregate_daily_precipitation(
  input_file = "short.csv",
  output_file = "daily_precipitation.csv",
  threshold = 0.1
)

# Step 3: Apply mass-balanced stretching
stretched_data <- calculate_mass_balanced_stretched_precipitation(
  input_file = "daily_precipitation.csv",
  output_file = "precipitation_stretched.csv",
  threshold_percentile = 95,
  stretch_amount = 1.0,  # 100% increase for extreme events
  tolerance = 0.001
)

# Step 4: Examine results
summary(stretched_data)
```

## Output File Descriptions

### CSV Outputs
All stretching functions produce CSV files with these columns:
- `Date` - Date in YYYY-MM-DD format
- `Precipitation` - Original precipitation (mm)
- `Percentile` - Percentile rank (0-100, NA for zero days)
- `StretchedPrecipitation` - Modified precipitation (mm)
- `StretchRatio` - Ratio of stretched/original (NA for zero days)

### JSON Metadata
Each function generates comprehensive metadata including:
- **Parameters used** - All input parameters for reproducibility
- **Processing statistics** - Record counts, date ranges, totals
- **Quality metrics** - Mass balance errors, convergence info
- **Generation info** - Timestamp, R version, script details

## Dependencies

- **Base R** - Core functionality
- **lubridate** - Date/time processing (auto-installed if missing)
- **jsonlite** - JSON output (optional, falls back to manual JSON)

## Best Practices

1. **Start with aggregation**: Always convert sub-daily data to daily totals first
2. **Check your data**: Verify date formats and column names match requirements
3. **Use mass-balanced stretching**: Recommended for research applications
4. **Preserve metadata**: Keep JSON files with your CSV outputs for reproducibility
5. **Test parameters**: Start with conservative stretching amounts and adjust gradually
6. **Validate results**: Check mass balance errors and stretch ratios for reasonableness

## Troubleshooting

**Common Issues:**
- **Date parsing errors**: Ensure dates are in "YYYY-MM-DD HH:MM" format
- **Missing columns**: Input CSVs must have exact column names (`date`, `Precipitation`, etc.)
- **Convergence failures**: Reduce tolerance or increase max_iterations for difficult datasets
- **Memory issues**: For very large datasets, consider processing in chunks

**Parameter Guidance:**
- `threshold_percentile`: 90-99 (95 is typical for extreme events)
- `stretch_amount`: 0.1-2.0 (0.5-1.0 recommended for most applications)  
- `scaling_factor`: 0.5-2.0 (1.0 provides linear scaling)

