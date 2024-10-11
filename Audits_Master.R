# Intro --------------------------
"
This is the master file which replicates the entire analysis of the paper. It calls the following
scripts in order:

1. grf_pre_cleaning.R -- this script imports tax returns and audit data, then creates the dataset
used to train machine learning algorithms which is saved at:
~/Pakistan Audits/Data/tax_returns_data_ML_cleaned.csv

2. GRF_train_main.R -- this script imports the cleaned data and conducts ML training. The resulting
models are saved to:
~/Pakistan Audits/Large_Output/cf_model_trained_2017.rda
This script also outputs a dataset with treatment effect predictions and doubly-robust scores
that is used in optimization.

3. MVPF_optimization.R -- imports the dataset from the ML script and conducts optimization for 
each policy and each welfare preference parameter. Will output the dataset with optimal policy 
allocations.

4. NEW (TBD): Policy_Comparisons.R -- imports the dataset from the MVPF_optimization.R script 
and creates tables and figures related to the optimal vs. observed policies.

"

rm(list = ls())

# Calls script for cleaning ------------------------------

source(file.path("C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/build/code/grf_pre_cleaning.R"))

cat("Data Processing Complete.\n")

# Calls script for GRF training ----------------------------

source(file.path("C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/code/GRF_train_main.R"))

cat("GRF Training Complete.\n")

# Calls script for policy optimization -------------------------

source(file.path("C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/code/MVPF_optimization.R"))

cat("All Optimization Complete.\n")











