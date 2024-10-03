# Intro -----------------------------

"
This script performs final data cleaning before ML exercises can be performed
"

rm(list = ls())

library(tidyverse)
library(haven)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"

# Import 2013-2014 tax returns 
tax_returns_13.14 <- read_dta(file.path(data_path, "Individual_ITReturns_2013_2014_cleaned.dta"))




















