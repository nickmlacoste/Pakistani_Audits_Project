# Intro -----------------------------

"
This script takes the data with optimal and observed policies and creates tables and figures
relevant to describing the MVPFs/revenues/costs under observed vs. optimal policies vs. 
overlap policies, the target subsample characteristics, etc.
"

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output/tables"

# Import 2013-2021 tax returns 
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_post_opt.csv"))

#generating a random "observed" treatment sequence (DELETE ONCE THE DATASET IS REAL)
tax_returns_df$audited_current <- sample(c(0, 1), size = nrow(tax_returns_df), 
                                         replace = TRUE, prob = c(0.9, 0.1))













