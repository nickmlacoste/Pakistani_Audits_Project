# Intro -----------------------------

"
This script performs final data cleaning before ML exercises can be performed
"

rm(list = ls())

library(tidyverse)
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

tax_returns_df <- read.csv(file.path(output_path, "tax_returns_test_data.csv"))

# Creating derived columns that need to be created before pivoting -------------------------

tax_returns_df <- tax_returns_df %>%
  mutate(
    duedate = as.Date(duedate),
    filingdate = as.Date(filingdate),
    days_late = as.numeric(filingdate - duedate)
  ) %>%
  select(-individual)

#view any derived columns
tax_returns_df %>% pull(days_late)



# Pivot dataframe wider ------------------

tax_returns_df <- tax_returns_df %>%
  pivot_wider(
    id_cols = ntn,                      # create 1 row per taxpayer id
    names_from = year,                   # Pivot columns based on 'year'
    values_from = -c(ntn, year),          # Pivot all columns other than ID and year
    names_sep = "_",                     # Separator for new column names
    names_glue = "{.value}_{year}"        # Format new column names as variable_year
  )


# Create NPV of tax revenue for a given year -------------------------

calculate_npv <- function(df, year) {
  discount_rate <- 0.03
  npv <- 0
  
  for (i in 0:3) {
    future_year <- year + i
    column_name <- paste0("nettaxchargeable_", future_year)
    
    # Replace NA with 0
    value <- ifelse(is.na(df[[column_name]]), 0, df[[column_name]])
    
    # Calculate discounted value and add to NPV
    npv <- npv + value / ((1 + discount_rate)^i)
  }
  
  return(round(npv, 2))
}

# Apply the function to each year and create new columns
years <- 2017:2018
for (year in years) {
  npv_column_name <- paste0("NPV_taxrevenue_", year)
  tax_returns_df[[npv_column_name]] <- map_dbl(1:nrow(tax_returns_df), ~ calculate_npv(tax_returns_df[., ], year))
}

View(tax_returns_df %>%
  select(starts_with("nettaxchargeable_"), starts_with("NPV_taxrevenue_")))



# Generate an "audited indicator" (TEMPORARY) --------------------------

# here I randomly assign each person a 10% chance of being audited each year
generate_audited_indicator <- function(n) {
  return(ifelse(runif(n) < 0.1, 1, 0))
}

# Apply the function to each year and create new columns
years <- 2013:2021
for (year in years) {
  audited_column_name <- paste0("audited_", year)
  tax_returns_df[[audited_column_name]] <- generate_audited_indicator(nrow(tax_returns_df))
}


# Calculate taxpayer burden (UPDATE THIS WHEN YOU GET BURDEN SURVEY DATA) -------------------------

# Function to calculate burden for a given year
calculate_burden <- function(df, year) {
  audited_column <- paste0("audited_", year)
  income_column <- paste0("taxableinc_", year)
  burden_column <- paste0("burden_", year)
  
  # Calculate average taxable income for the year
  avg_income <- mean(df[[income_column]], na.rm = TRUE)
  
  # Rank the taxable income for the year
  df <- df %>%
    mutate(rank = rank(df[[income_column]], ties.method = "average"))
  
  # Burden increases linearly from 100 to 6000 based on income, 3000 correspods to average income
  # Burden = 0 if not audited, if taxable income is missing and audit = 1 then they get 3000 as burden
  df[[burden_column]] <- ifelse(df[[audited_column]] == 0, 0,
                                ifelse(is.na(df[[income_column]]), 3000,
                                       ifelse(df[[income_column]] == avg_income, 3000,
                                              100 + (df$rank - 1) / (nrow(df) - 1) * (6000 - 100))))
  
  # Remove the temporary rank column
  df <- df %>% select(-rank)
  
  return(df)
}

# Apply the function to each year and create new columns
years <- 2014:2021
for (year in years) {
  tax_returns_df <- calculate_burden(tax_returns_df, year)
}


# Generate audit costs (TEMPORARY UNTIL WE MERGE IN ACTUAL COST DATA) -------------------------

# Function to calculate audit cost for a given year
calculate_audit_cost <- function(df, year) {
  audited_column <- paste0("audited_", year)
  income_column <- paste0("taxableinc_", year)
  audit_cost_column <- paste0("audit_cost_", year)
  
  # Rank the taxable income for the year
  df <- df %>%
    mutate(rank = rank(df[[income_column]], ties.method = "average"))
  
  # I have audit costs increasing linearly from 100 to 10,000 based on the person's income
  # Audit cost is of course = 0 if they are not audited
  df[[audit_cost_column]] <- ifelse(df[[audited_column]] == 0, 0,
                                    100 + (df$rank - 1) / (nrow(df) - 1) * (10000 - 100))
  
  # Remove the temporary rank column
  df <- df %>% select(-rank)
  
  return(df)
}

# Apply the function to each year and create new columns
years <- 2014:2021
for (year in years) {
  tax_returns_df <- calculate_audit_cost(tax_returns_df, year)
}

# Write ML-ready data to the data path -----------------------------

write.csv(tax_returns_df, file.path(output_path, "tax_returns_data_ML_cleaned.csv"), row.names = FALSE)














