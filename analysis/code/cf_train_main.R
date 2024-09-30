rm(list = ls())

library(tidyverse)
library(haven)
library(grf)
library(xtable)
library(plm)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/Charter_School_Heterogeneity_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Charter School Heterogeneity/large_output"

# Import test score data
#charter_seda <- read_dta(file.path(data_path, "charter_seda.dta"))
charter_seda <- read_dta(file.path(data_path, "charter_seda_c.dta")) #this one is fully cleaned
