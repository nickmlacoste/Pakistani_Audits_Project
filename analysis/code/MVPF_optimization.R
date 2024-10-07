# Intro -----------------------------

"
This script performs optimization. We consider 4 policies:
1. Minimize MVPF such that revenue >= observed revenue
2. Miniimze MVPF such that revenue >= observed revenue and cost <= observed cost
3. Maximize revenue such that MVPF <= observed MVPF
4. Maximize revenue such that MVPF <= observed MVPF and cost <= observed cost
"

rm(list = ls())

library(tidyverse)


data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
figures_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistan_Audits_Project/analysis/output"

# Import data with GRF predictions
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_post_ML.csv"))





































