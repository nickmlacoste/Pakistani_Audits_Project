# Intro -----------------------------

"
This script converts all batch csv files into binary (parquet) files. Then calls grf_pre_cleaning.R
on the binary version of the data, which is compressed and should work.
"

library(dplyr)
library(readr)
library(stringr)
library(arrow)
library(furrr)  # For parallel processing
library(future) # For setting up parallel plan
library(forcats)
library(ggplot2)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)
library(haven)
library(readxl)

# Define paths
data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data/Raw_Batched"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data/Pre_ML_Batched"
code_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/build/code"

# List all batch files in the Raw_Batched folder, ensure they're in order
batch_files <- list.files(path = data_path, pattern = "tax_return_batch_.*\\.csv", full.names = TRUE)
batch_files <- batch_files[order(as.numeric(str_extract(basename(batch_files), "\\d+")))]

# Define columns to exclude from numeric conversion
non_convert_cols <- c("tid", "audit_income", "audit_tax", "audited", "duedate",
                      "sentdate", "documentdate", "taxpayertype", "residentstatus",
                      "ty", "subject", "medium")

# Function to clean and convert each CSV file to Parquet
convert_to_parquet <- function(file) {
  # Load the dataset
  tax_returns_df <- read_csv(file, show_col_types = FALSE)
  
  # Apply cleaning operation: remove spaces and commas, then convert to numeric
  tax_returns_df <- suppressWarnings(
    tax_returns_df %>%
      mutate(tid = as.character(tid)) %>%
      mutate(across(
        .cols = -any_of(non_convert_cols),  # Apply to all columns except the excluded ones
        .fns = ~ as.numeric(gsub(",", "", gsub("^\\s+", "", .)))  # Remove spaces and commas, then convert to numeric
      ))
  )
  
  # Save the cleaned file as a Parquet in the output folder
  output_parquet <- file.path(output_path, paste0("converted_", tools::file_path_sans_ext(basename(file)), ".parquet"))
  write_parquet(tax_returns_df, output_parquet)
  
  # Remove dataset from memory after saving
  rm(tax_returns_df)
  gc()  # Garbage collection to free memory
  
  # Return the path of the Parquet file for tracking
  return(output_parquet)
}

# Convert each CSV to Parquet one at a time and collect Parquet file paths
parquet_files <- map_chr(batch_files, convert_to_parquet)

# Now that all CSVs are converted, merge all Parquet files into a single dataset
final_dataset <- bind_rows(future_map(parquet_files, read_parquet))

# Save the combined dataset as a single Parquet file
final_parquet <- file.path(output_path, "tax_returns_data_ML_combined.parquet")
write_parquet(final_dataset, final_parquet)

# Calculate bc_summary after full dataset processing
bc_summary <- data.frame(year = integer(), avg_cost = numeric(), bc_ratio = numeric())
target_burden_at_mean <- 520085

calculate_bc_summary <- function(df, year) {
  audited_column <- paste0("audited_", year)
  audit_cost_column <- paste0("audit_cost_", year)
  
  # Calculate average cost for audited individuals
  avg_cost <- mean(df[[audit_cost_column]][df[[audited_column]] == 1], na.rm = TRUE)
  
  # Calculate the B/C ratio
  bc_ratio <- target_burden_at_mean / avg_cost
  
  # Append to bc_summary
  bc_summary <<- rbind(bc_summary, data.frame(year = year, avg_cost = avg_cost, bc_ratio = bc_ratio))
}

# Apply bc_summary calculation for each year
for (year in 2014:2021) {
  calculate_bc_summary(final_dataset, year)
}

# Save bc_summary table
write.csv(bc_summary, file.path(output_path, "bc_summary.csv"), row.names = FALSE)

cat("All batches converted to Parquet, combined, and saved as a single Parquet file.\n")

# Clear memory of intermediate datasets
rm(final_dataset)
gc()

# Load the combined Parquet file and apply the full cleaning in grf_pre_cleaning.R
final_dataset <- read_parquet(final_parquet)

# Assign to tax_returns_df as expected by grf_pre_cleaning.R
tax_returns_df <<- final_dataset
source(file.path(code_path, "grf_pre_cleaning.R"))

# Save the fully cleaned dataset
write_parquet(tax_returns_df, file.path(output_path, "tax_returns_data_ML_cleaned.parquet"))
cat("Full dataset processed and saved as a final cleaned Parquet file.\n")
