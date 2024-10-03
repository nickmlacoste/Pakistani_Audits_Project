# Intro -----------------------------

"
This script produces the causal forest estimates of individualized treatment effects (and any other ML 
algorithms we may use for that) for the Pakistani Audits paper.  
"

rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistan_Audits_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Large_Output"

# Import 2013-2014 tax returns 
tax_returns_13.14 <- read_dta(file.path(data_path, "Individual_ITReturns_2013_2014_cleaned.dta"))




























