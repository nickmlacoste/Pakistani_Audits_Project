# Intro -----------------------------

"
This script calls the grf_pre_cleaning.R script in batches 
"

library(dplyr)
library(readr)
library(stringr)
library(arrow)  
library(forcats)
library(ggplot2)
library(lubridate)
library(purrr)
library(tibble)
library(tidyr)

# Define paths
data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data/Raw_Batched"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data/Pre_ML_Batched"

# List all batch files in the Raw_Batched folder, ensure they're in order
batch_files <- list.files(path = data_path, pattern = "tax_return_batch_.*\\.csv", full.names = TRUE)
batch_files <- batch_files[order(as.numeric(str_extract(basename(batch_files), "\\d+")))]

# Loop through each batch file, apply cleaning, and save as both CSV and Parquet
for (file in batch_files) {
  # Load the dataset
  tax_returns_df <- read_csv(file, show_col_types = FALSE)
  
  # Source the grf_pre_cleaning.R script to perform additional cleaning on tax_returns_df
  source("grf_pre_cleaning.R")
  
  # Save the cleaned batch as a .csv file in the output folder
  output_csv <- file.path(output_path, paste0("cleaned_", basename(file)))
  write_csv(tax_returns_df, output_csv)
  
  # Save the cleaned batch as a Parquet file in the output folder
  output_parquet <- file.path(output_path, paste0("cleaned_", tools::file_path_sans_ext(basename(file)), ".parquet"))
  write_parquet(tax_returns_df, output_parquet)
  
  # Clear memory for the next iteration
  rm(tax_returns_df)
  gc()
}

# List all saved Parquet files in the output folder
parquet_files <- list.files(path = output_path, pattern = "cleaned_.*\\.parquet", full.names = TRUE)

# Read and combine all Parquet files into a single dataset
final_dataset <- bind_rows(lapply(parquet_files, read_parquet))

# Save the combined dataset as a single Parquet file
write_parquet(final_dataset, file.path(output_path, "tax_returns_data_ML_cleaned.parquet"))

cat("All batches processed, saved, and merged into a single Parquet file.\n")

