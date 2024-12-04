library(arrow)
library(data.table)

# Define paths
data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data/Pre_ML_Batched"
output_file <- file.path(data_path, "tax_returns_data_ML_combined_incremental.parquet")

# List all Parquet files in the directory, sorted
parquet_files <- list.files(path = data_path, pattern = "\\.parquet$", full.names = TRUE)
parquet_files <- parquet_files[order(as.numeric(stringr::str_extract(basename(parquet_files), "\\d+")))]

# Create the combined file by writing each Parquet file incrementally
for (i in seq_along(parquet_files)) {
  # Read in each Parquet file as a data.table to optimize memory usage
  chunk <- as.data.table(read_parquet(parquet_files[i]))
  
  # If it's the first file, initialize the Parquet file; otherwise, append
  if (i == 1) {
    write_parquet(chunk, output_file)  # Create the initial combined file
  } else {
    # Append each subsequent file's data to the output Parquet file
    write_parquet(chunk, output_file, append = TRUE)
  }
  
  # Clear memory for the next chunk
  rm(chunk)
  gc()  # Garbage collection to free memory
}

cat("Incremental combination of all Parquet files saved to tax_returns_data_ML_combined_incremental.parquet.\n")









