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
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_data_ML_cleaned.csv"))

# Estimate multi-arm causal forest --------------------------

X_covariates <- c("taxableinc_2014", "incfrsalary_2014", "incfrbusiness_2014")

# remove rows with missing data for Y or W
tax_returns_df <- tax_returns_df %>%
  filter(!is.na(audited_2014) & 
           !is.na(NPV_taxrevenue_2014) & 
           !is.na(audit_cost_2014) &
           !is.na(burden_2014))

# need to define the training objects as matrices/vectors
X_matrix <- as.matrix(tax_returns_df[, X_covariates])
Y_matrix <- as.matrix(tax_returns_df[, c("NPV_taxrevenue_2014", "audit_cost_2014", "burden_2014")])
W_vector <- factor(as.vector(tax_returns_df$audited_2014))


# train GRF

cf_model <- multi_arm_causal_forest(X = X_matrix,
                                    Y = Y_matrix,
                                    W = W_vector,
                                    Y.hat = NULL,
                                    W.hat = NULL,
                                    num.trees = 1000
                                    )

# Save the causal forest model
saveRDS(cf_model, file = paste0(cf_output_path, "/cf_model_trained.rda"))
























