# Intro -----------------------------

"
This script performs final data cleaning before ML exercises can be performed.
"

rm(list = ls())

#library(tidyverse)
library(dplyr)
library(forcats)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)
library(haven)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"

# Import raw tax return data 

# # For now I'm cutting each dataset to their first 100 rows. This is for ease of code testing
# tax_returns_df <- bind_rows(
#   read_dta(file.path(data_path, "Individual_ITReturns_2013_2014_cleaned.dta")) %>% slice(1:100),
#   read_dta(file.path(data_path, "Individual_ITReturns_2015_2016_cleaned.dta")) %>% slice(1:100),
#   read_dta(file.path(data_path, "Individual_ITReturns_2017_2018_cleaned.dta")) %>% slice(1:100),
#   read_dta(file.path(data_path, "Individual_ITReturns_2019_2020_cleaned.dta")) %>% slice(1:100)
# )

# write.csv(tax_returns_df, file.path(data_path, "tax_returns_test_data.csv"), row.names = FALSE)

#tax_returns_df <- read.csv(file.path(output_path, "tax_returns_test_data.csv"))
tax_returns_df <- read_dta(file.path(output_path, "tax_returns_test_data.dta"))

# Creating derived columns that need to be created before pivoting -------------------------

# Convert all columns that look like numbers to numeric
non_convert_cols <- c("tid", "audit_income", "audit_tax", "audited", "duedate",
                      "sentdate", "documentdate", "taxpayertype", "residentstatus",
                      "ty", "subject", "medium")
tax_returns_df <- suppressWarnings(
  tax_returns_df %>%
  mutate(across(
    .cols = -any_of(non_convert_cols),  # Apply to all columns except the excluded ones
    .fns = ~ as.numeric(gsub(",", "", gsub("^\\s+", "", .)))  # Remove spaces and commas, then convert to numeric
  ))
)


tax_returns_df <- tax_returns_df %>%
  mutate(
    audited = if_else(is.na(audited), 0, audited),
    duedate = as.Date(duedate, format = "%d-%b-%y"),
    sentdate = as.Date(sentdate, format = "%d-%b-%y"),
    days_late = as.numeric(sentdate - duedate),
    totalincomeassessed = if_else(audited == 0, 0, 
                                  if_else(is.na(totalincomeassessed), 0, totalincomeassessed)),
    audit_income = if_else(audited == 0, 0, 
                           if_else(is.na(audit_income), 0, audit_income)),
    demandedincometax = if_else(audited == 0, 0, 
                                if_else(is.na(demandedincometax), 0, demandedincometax)),
    audit_tax = if_else(audited == 0, 0, if_else(is.na(audit_tax), 0, audit_tax))
  ) %>%
  rename(year = ty) %>%
  select(-c(taxpayertype, subject, `_merge`)) # enter any unused columns

#view any derived columns
tax_returns_df %>% pull(days_late)



# Pivot dataframe wider ------------------

tax_returns_df <- tax_returns_df %>%
  pivot_wider(
    id_cols = tid,                      # create 1 row per taxpayer id
    names_from = year,                   # Pivot columns based on 'year'
    values_from = -c(tid, year),          # Pivot all columns other than ID and year
    names_sep = "_",                     # Separator for new column names
    names_glue = "{.value}_{year}"        # Format new column names as variable_year
  )

# drop rows where nettaxchargeable_year is missing for any of 2017-2021: 
# this indicates the taxpayer didn't file a return in that year. Missing values are predictors
# for 2012-2016, but a problem in 2017-2021 because imputing 0 understates their NPV
tax_returns_df <- tax_returns_df %>%
  filter(!if_any(all_of(paste0("nettaxchargeable9200_", 2017:2021)), is.na))

#View(tax_returns_df %>% select(starts_with("nettaxchargeable9200_")))


# Create NPV of tax revenue for a given year -------------------------

calculate_npv_taxrevenue <- function(df, base_year) {
  discount_rate <- 0.03
  npv <- 0
  
  # Direct impact: audit_tax in the base year
  audit_tax_column <- paste0("audit_tax_", base_year)
  audit_tax_value <- ifelse(is.na(df[[audit_tax_column]]), 0, df[[audit_tax_column]])
  
  # Initialize NPV with the audit_tax for the base year
  npv <- npv + audit_tax_value
  
  # Base year nettaxchargeable value
  base_nettax_column <- paste0("nettaxchargeable9200_", base_year)
  base_nettax_value <- ifelse(is.na(df[[base_nettax_column]]), 0, df[[base_nettax_column]])
  
  # Calculate the deterrence effect over the next 3 years (relative to the base year)
  for (i in 1:3) {
    future_year <- base_year + i
    future_nettax_column <- paste0("nettaxchargeable9200_", future_year)
    
    # Calculate declared_tax_change (difference between future year and base year)
    declared_tax_change <- ifelse(is.na(df[[future_nettax_column]]), 0, df[[future_nettax_column]]) -
      base_nettax_value
    
    # Discount the declared_tax_change and add it to the NPV
    npv <- npv + (declared_tax_change / ((1 + discount_rate)^i))
  }
  
  return(round(npv, 2))
}

# Apply the function to each year and create new columns
years <- 2017:2018
for (year in years) {
  npv_column_name <- paste0("NPV_taxrevenue_", year)
  tax_returns_df[[npv_column_name]] <- map_dbl(1:nrow(tax_returns_df), ~ calculate_npv_taxrevenue(tax_returns_df[., ], year))
}


#View(tax_returns_df %>% select(starts_with("nettaxchargeable9200_"), starts_with("NPV_taxrevenue_")))


# Generate audit costs (TEMPORARY UNTIL WE MERGE IN ACTUAL COST DATA) -------------------------

# Function to calculate audit cost for a given year
calculate_audit_cost <- function(df, year) {
  audited_column <- paste0("audited_", year)
  income_column <- paste0("taxableincome9100_", year)
  audit_cost_column <- paste0("audit_cost_", year)
  
  # Rank the taxable income for the year
  df <- df %>%
    mutate(rank = rank(df[[income_column]], ties.method = "average"))
  
  # Set audit costs to range from 1,000,000 to 10,000,000 based on income
  df[[audit_cost_column]] <- ifelse(df[[audited_column]] == 0, 0,
                                    1000000 + (df$rank - 1) / (nrow(df) - 1) * (10000000 - 1000000))
  
  # Remove the temporary rank column
  df <- df %>% select(-rank)
  
  return(df)
}

# Apply the function to each year and create new columns
years <- 2014:2021
for (year in years) {
  tax_returns_df <- calculate_audit_cost(tax_returns_df, year)
}

# Calculate taxpayer burden  -------------------------

bc_summary <- data.frame(year = integer(), avg_cost = numeric(), bc_ratio = numeric())

# Function to calculate burden for a given year
calculate_burden <- function(df, year) {
  audited_column <- paste0("audited_", year)
  income_column <- paste0("taxableincome9100_", year)
  burden_column <- paste0("burden_", year)
  audit_cost_column <- paste0("audit_cost_", year)
  
  # Define target average burden
  target_burden_at_mean <- 520085
  
  # Calculate average cost for audited individuals
  avg_cost <- mean(df[[audit_cost_column]][df[[audited_column]] == 1], na.rm = TRUE)
  
  # Calculate the B/C ratio
  bc_ratio <- target_burden_at_mean / avg_cost
  
  # Record the average cost and B/C ratio for the year
  bc_summary <<- rbind(bc_summary, data.frame(year = year, avg_cost = avg_cost, bc_ratio = bc_ratio))
  
  # Calculate average taxable income for the year
  avg_income <- mean(df[[income_column]], na.rm = TRUE)
  
  # Define slope for income adjustment
  slope <- 0.18
  
  # Calculate burden for each individual
  df[[burden_column]] <- ifelse(df[[audited_column]] == 0, 0,
                                ifelse(is.na(df[[income_column]]), target_burden_at_mean,
                                       # Initial burden based on B/C ratio and audit cost
                                       (df[[audit_cost_column]] * bc_ratio) +
                                         # Income adjustment based on log difference from avg income
                                         slope * (log(df[[income_column]]) - log(avg_income))))
  
  return(df)
}

# Apply the function to each year and create new columns
years <- 2014:2021
for (year in years) {
  tax_returns_df <- calculate_burden(tax_returns_df, year)
}

# View and save the B/C summary table
write.csv(bc_summary, file.path(output_path, "bc_summary.csv"), 
          row.names = FALSE)


# Write ML-ready data to the data path -----------------------------

write.csv(tax_returns_df, file.path(output_path, "tax_returns_data_ML_cleaned.csv"), 
          row.names = FALSE)














