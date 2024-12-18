# Intro -----------------------------

"
This script produces the causal forest estimates of individualized treatment effects (and any other ML 
algorithms we may use for that) for the Pakistani Audits paper.  
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
library(grf)
library(knitr)
library(kableExtra)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
figures_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Large_Output"

# Import 2013-2021 tax returns 
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_data_ML_cleaned.csv"))

# Last-minute cleaning ------------------------------


# Split data by year (for training and out-of-bag predicting)
# Train on 2017 (randomized), test on 2018 (can observe 4-year NPVs in that year only)
# NOTE: the predictive model can be extrapolated to any future year, but we can only observe true NPVs
# in the year 2018 because the data stops at 2021 
target_years <- c(2017, 2018)

for (year in target_years) {
  
  # Define the three years before and the three years after the current year
  year_b1 <- year - 1
  year_b2 <- year - 2
  year_b3 <- year - 3
  year_a1 <- year + 1
  year_a2 <- year + 2
  year_a3 <- year + 3
  
  # Create the dataframe name dynamically (e.g., tax_returns_df_17 for 2017)
  df_name <- paste0("tax_returns_df_", substr(year, 3, 4))
  
  # Select columns that include the current year, the 3 years before, and the 3 years after, always keep 'tid'
  selected_columns <- tax_returns_df %>%
    select(
      tid,
      ends_with(as.character(year_b3)), 
      ends_with(as.character(year_b2)),
      ends_with(as.character(year_b1)),
      ends_with(as.character(year)),
      ends_with(as.character(year_a1)),
      ends_with(as.character(year_a2)),
      ends_with(as.character(year_a3))
    )
  
  # Rename the columns: years before current year are _b1, _b2, _b3; years after are _a1, _a2, _a3
  renamed_df <- selected_columns %>%
    rename_with(~ gsub(paste0("_", year_a3), "_a3", .), ends_with(as.character(year_a3))) %>%
    rename_with(~ gsub(paste0("_", year_a2), "_a2", .), ends_with(as.character(year_a2))) %>%
    rename_with(~ gsub(paste0("_", year_a1), "_a1", .), ends_with(as.character(year_a1))) %>%
    rename_with(~ gsub(paste0("_", year), "_current", .), ends_with(as.character(year))) %>%
    rename_with(~ gsub(paste0("_", year_b1), "_b1", .), ends_with(as.character(year_b1))) %>%
    rename_with(~ gsub(paste0("_", year_b2), "_b2", .), ends_with(as.character(year_b2))) %>%
    rename_with(~ gsub(paste0("_", year_b3), "_b3", .), ends_with(as.character(year_b3)))
  
  # Assign the dataframe to the dynamically created name (e.g., tax_returns_df_17 for 2017)
  assign(df_name, renamed_df)
  
  rm(renamed_df, selected_columns)
  
  # Print confirmation
  cat("Created dataset:", df_name, "\n")
}

# filtration operations: remove those treated after the current year for training and testing,
# then remove all _a1, _a2, _a3 variables (except revenue) as they are not needed for anything
tax_returns_df_17 <- tax_returns_df_17 %>%
  filter(!(audited_a1 == 1 | audited_a2 == 1 | audited_a3 == 1)) %>%
  select(-matches(".*_a1$|.*_a2$|.*_a3$"), 
         matches("NPV_taxrevenue_a1$|NPV_taxrevenue_a2$|NPV_taxrevenue_a3$|^tid$"))
tax_returns_df_18 <- tax_returns_df_18 %>%
  filter(!(audited_a1 == 1 | audited_a2 == 1 | audited_a3 == 1)) %>%
  select(-matches(".*_a1$|.*_a2$|.*_a3$"), 
         matches("NPV_taxrevenue_a1$|NPV_taxrevenue_a2$|NPV_taxrevenue_a3$|^tid$"))


# Estimate full multi-arm causal forest --------------------------

# function for interactive covariate selection
generate_column_names <- function(df, prefix_selection) {
  all_columns <- colnames(df)
  
  # Generate patterns for the selected prefixes
  include_patterns <- names(prefix_selection)[unlist(prefix_selection)]
  exclude_patterns <- names(prefix_selection)[!unlist(prefix_selection)]
  
  # Omit _current if 'audited', 'audit_tax', and 'audit_income' prefixes are selected
  if (any(c("audited", "audit_tax", "audit_income") %in% include_patterns)) {
    # Remove the prefixes from 'include_patterns'
    include_patterns <- setdiff(include_patterns, c("audited", "audit_tax", "audit_income"))
    
    # Select columns with '_b1', '_b2', or '_b3' for 'audited', 'audit_tax', and 'audit_income'
    audited_columns <- grep("^(audited|audit_tax|audit_income)_(b1|b2|b3)$", all_columns, value = TRUE)
  } else {
    audited_columns <- character(0)
  }
  
  include_regex <- paste0("^(", paste(include_patterns, collapse = "|"), ")_(current|b1|b2|b3)$")
  exclude_regex <- paste0("^(", paste(exclude_patterns, collapse = "|"), ")_(current|b1|b2|b3)$")
  
  # Include columns with the selected prefixes
  selected_columns <- all_columns[grepl(include_regex, all_columns)]
  
  # Exclude columns with the selected prefixes
  if (length(exclude_patterns) > 0) {
    selected_columns <- selected_columns[!grepl(exclude_regex, selected_columns)]
  }
  
  # Add the special 'audited' columns
  selected_columns <- c(selected_columns, audited_columns)
  
  return(selected_columns)
}

# List of prefixes: select TRUE if you wish to include, FALSE if not
# note this will include all instances of *_b3, *_b2, *_b1, *_current in the X matrix
prefix_selection <- list(
  residentstatus = TRUE,
  sentdate = FALSE,
  documentdate = FALSE,
  duedate = FALSE,
  medium = TRUE,
  receiptvalues640001 = TRUE,
  fixfinaltax640001 = TRUE,
  fixfinaltaxdeducted640001 = TRUE,
  totalincome9000 = TRUE,
  salaryincome1000 = TRUE,
  taxableincome9100 = TRUE,
  propertyincome2000 = TRUE,
  businessincome3000 = TRUE,
  capitalassetincome4000 = TRUE,
  othersourceincome5000 = TRUE,
  foreignincome6000 = TRUE,
  agricultureincome6100 = TRUE,
  exemptincome = TRUE,
  turnover = TRUE,
  costofsales3030 = TRUE,
  grossprofitloss3100 = TRUE,
  accountingprofitloss3200 = TRUE,
  nettaxchargeable9200 = TRUE,
  minimumtax = TRUE,
  normalincometax920000 = TRUE,
  withholdingincometax9201 = TRUE,
  fixfinaltax920100 = TRUE,
  advanceincometax9202 = TRUE,
  admittedtax9203 = TRUE,
  demandedincometax9204 = TRUE,
  supertax923181 = TRUE,
  refund9210 = TRUE,
  exports640000 = FALSE,
  exports640001 = FALSE,
  us1531acola640000 = FALSE,
  us1531acolb640000 = FALSE,
  us1531acolc640000 = FALSE,
  us1531acola640001 = FALSE,
  us1531acolb640001 = FALSE,
  us1531acolc640001 = FALSE,
  us1531bcola640000 = FALSE,
  us1531bcolb640000 = FALSE,
  us1531bcolc640000 = FALSE,
  us1531bcola640001 = FALSE,
  us1531bcolb640001 = FALSE,
  us148cola640000 = FALSE,
  us148colb640000 = FALSE,
  us148colc640000 = FALSE,
  us148cola640001 = FALSE,
  us148colb640001 = FALSE,
  us148colc640001 = FALSE,
  taxcredit9329 = TRUE,
  agricultureincometax9291 = TRUE,
  netrevenue3029 = TRUE,
  grossrevenue3009 = TRUE,
  sellingexpenses3019 = TRUE,
  openingstock3039 = TRUE,
  importedrawmaterial3036 = TRUE,
  netpurchasesexcludingtax3059 = TRUE,
  netdomesticpurchases3055 = TRUE,
  netimportrawmaterial3056 = TRUE,
  consumed3069 = TRUE,
  domesticrawmaterial3065 = TRUE,
  importrawmaterial3066 = TRUE,
  directexpenses3089 = TRUE,
  salaries3071 = TRUE,
  closingstock3099 = TRUE,
  managementadministrativesellingf = TRUE,
  salaries3154 = TRUE,
  incomelossfrombusiness3270 = TRUE,
  unadjustedlossesfrompreviousyear = TRUE,
  totalassets3349 = TRUE,
  building3302 = TRUE,
  plantmachinery3303 = TRUE,
  capitalworkinprogress3308 = TRUE,
  cashcashequivalents3319 = TRUE,
  liabilities3399 = TRUE,
  authorizedcapital3351 = TRUE,
  paidupcapital3352 = TRUE,
  accountingprofittaxchargeable923 = TRUE,
  totaltaxdeductedundersection6408 = TRUE,
  totaltaxdeductedundersection6410 = TRUE,
  audited = TRUE,
  audit_income = TRUE,
  audit_tax = TRUE,
  days_late = TRUE
)


# Generate the list of column names to include
X_covariates <- generate_column_names(tax_returns_df_17, prefix_selection)

# remove rows with missing data for Y or W
tax_returns_df_17 <- tax_returns_df_17 %>%
  filter(!is.na(audited_current) & 
           !is.na(NPV_taxrevenue_current) & 
           !is.na(audit_cost_current) &
           !is.na(burden_current))

tax_returns_df_18 <- tax_returns_df_18 %>%
  filter(!is.na(audited_current) & 
           !is.na(NPV_taxrevenue_current) & 
           !is.na(audit_cost_current) &
           !is.na(burden_current))

# need to define the training objects as matrices/vectors
X_matrix_train <- as.matrix(tax_returns_df_17[, X_covariates])
Y_matrix_train <- as.matrix(tax_returns_df_17[, c("NPV_taxrevenue_current", 
                                                  "audit_cost_current", 
                                                  "burden_current")])
W_vector_train <- factor(as.vector(tax_returns_df_17$audited_current)) #is the factor() function necessary here?

# treatment is random and sparse, so we provide GRF with known propensity scores to avoid 0's
propensity_score <- mean(tax_returns_df_17$audited_current)
W_hat_matrix <- matrix(c(1 - propensity_score, propensity_score), 
                       nrow = nrow(tax_returns_df_17), 
                       ncol = 2, byrow = TRUE)

# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_2017_model <- TRUE # 2017 data is from 2016 tax returns

if (train_2017_model) {
  
  cat("Training Primary Causal Forest \n")
  
  # Estimate the causal forest model
  cf_model_full_17 <- multi_arm_causal_forest(X = X_matrix_train,
                                              Y = Y_matrix_train,
                                              W = W_vector_train,
                                              Y.hat = NULL,
                                              W.hat = W_hat_matrix,
                                              num.trees = 1000,
                                              honesty = TRUE,
                                              honesty.fraction = 0.5,
                                              honesty.prune.leaves = TRUE,
                                              alpha = 0.05,
                                              imbalance.penalty = 0,
                                              stabilize.splits = TRUE,
                                              min.node.size = 5
                                              )
                           
  
  # Save the causal forest model
  saveRDS(cf_model_full_17, file = paste0(cf_output_path, "/cf_model_full_trained_2017.rda"))
  
  cat("Primary Causal Forest Trained and Saved \n")
  
} else {
  
  # Load the previously saved model
  cf_model_full_17 <- readRDS(file.path(cf_output_path, "cf_model_full_trained_2017.rda"))
  
  cat("Prior-Trained Primary Causal Forest Imported \n")
}

# Estimate evaluation forests for prediction quality tests -----------------------------

# need to define the training objects as matrices/vectors
X_matrix_train <- as.matrix(tax_returns_df_17[, X_covariates])
# only using revenue for RATE because multi-outcomes don't work
Y_matrix_train <- as.vector(tax_returns_df_17$NPV_taxrevenue_current)
W_vector_train <- as.vector(tax_returns_df_17$audited_current)

propensity_score <- mean(tax_returns_df_17$audited_current)
W_hat_vector <- rep(propensity_score, nrow(tax_returns_df_17))


# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_2017_test_models <- TRUE # 2017 data is from 2016 tax returns

n <- nrow(X_matrix_train)
train <- sample(1:n, n * 0.7) # training on 70% of the data
test <- setdiff(1:n, train) # evaluate on remaining 30%

if (train_2017_test_models) {
  
  cat("Training Causal Forests for Prediction Quality Tests \n")
  
  # Estimate the causal forest test and evaluation models
  cf_model_test <- causal_forest(X = X_matrix_train[train, ],
                                 Y = Y_matrix_train[train],
                                 W = W_vector_train[train],
                                 Y.hat = NULL,
                                 W.hat = W_hat_vector[train],
                                 num.trees = 1000,
                                 honesty = TRUE,
                                 honesty.fraction = 0.5,
                                 honesty.prune.leaves = TRUE,
                                 alpha = 0.05,
                                 imbalance.penalty = 0,
                                 stabilize.splits = TRUE,
                                 min.node.size = 5
                                 )
  
  cat("Test Forest Trained \n")
  
  cf_model_eval <- causal_forest(X = X_matrix_train[test, ],
                                 Y = Y_matrix_train[test],
                                 W = W_vector_train[test],
                                 Y.hat = NULL,
                                 W.hat = W_hat_vector[test],
                                 num.trees = 1000,
                                 honesty = TRUE,
                                 honesty.fraction = 0.5,
                                 honesty.prune.leaves = TRUE,
                                 alpha = 0.05,
                                 imbalance.penalty = 0,
                                 stabilize.splits = TRUE,
                                 min.node.size = 5
                                 )
  
  cat("Evaluation Forest Trained \n")
  
  
  # Save the causal forest models
  saveRDS(cf_model_test, file = paste0(cf_output_path, "/cf_model_test.rda"))
  saveRDS(cf_model_eval, file = paste0(cf_output_path, "/cf_model_evaluation.rda"))
  
  cat("Secondary Forests Saved \n")
  
} else {
  
  # Load the previously saved models
  cf_model_test <- readRDS(file.path(cf_output_path, "cf_model_test.rda"))
  cf_model_eval <- readRDS(file.path(cf_output_path, "cf_model_evaluation.rda"))
  
  cat("Prior-Trained Prediction Quality Causal Forests Imported \n")
}


# generate CATE predictions on 2017 returns data (year 2018) ----------------

#I'm also generating predictions for in-sample data using leave-one-out estimates
cate_hats_2017 <- as.data.frame(
  predict(
    object = cf_model_full_17,
    newdata = NULL,
    drop = TRUE,
    estimate.variance = FALSE
))
colnames(cate_hats_2017) <- c("NPV_revenue_cate", "audit_cost_cate", "burden_cate")

#these are the out-of-bag predictions used for policy derivation
cate_hats_2018 <- as.data.frame(
  predict(
    object = cf_model_full_17,
    newdata = tax_returns_df_18[, X_covariates],
    drop = TRUE, # removes the unnecessary dimension for multiple treat arms
    estimate.variance = FALSE
)$predictions)
colnames(cate_hats_2018) <- c("NPV_revenue_cate", "audit_cost_cate", "burden_cate")



# merge the predictions in with the 2018 data
tax_returns_df_18 <- tax_returns_df_18 %>%
  mutate(index = row_number())
cate_hats_2018 <- cate_hats_2018 %>%
  mutate(index = row_number())
tax_returns_df_18 <- left_join(tax_returns_df_18, cate_hats_2018, by = "index")

# Table for GRF predictions of MVPF from observed policy (vs. actual MVPF) -------------------

# Function to calculate MVPF
calculate_mvpfs <- function(df, R_col, C_col, B_col, W_col) {
  df_filtered <- df %>% filter(df[[W_col]] == 1)
  MVPF <- sum(df_filtered[[W_col]] * (df_filtered[[R_col]] + df_filtered[[B_col]])) / 
    sum(df_filtered[[W_col]] * (df_filtered[[R_col]] - df_filtered[[C_col]]))
  sum_revenue <- sum(df_filtered[[R_col]])
  sum_cost <- sum(df_filtered[[C_col]])
  sum_burden <- sum(df_filtered[[B_col]])
  return(list(MVPF = round(MVPF, 2), 
              sum_revenue = round(sum_revenue, 2),
              sum_cost = round(sum_cost,2),
              sum_burden = round(sum_burden,2)))
}

# Function to perform t-test and return significance stars
get_significance_stars <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Calculate observed values
observed_values <- calculate_mvpfs(tax_returns_df_18, 
                                   R_col = "NPV_taxrevenue_current", 
                                   C_col = "audit_cost_current", 
                                   B_col = "burden_current", 
                                   W_col = "audited_current")

# Calculate predicted values
predicted_values <- calculate_mvpfs(tax_returns_df_18, 
                                    R_col = "NPV_revenue_cate", 
                                    C_col = "audit_cost_cate", 
                                    B_col = "burden_cate", 
                                    W_col = "audited_current")

# Calculate differences and perform t-tests
differences <- c(
  predicted_values$MVPF - observed_values$MVPF,
  predicted_values$sum_revenue - observed_values$sum_revenue,
  predicted_values$sum_cost - observed_values$sum_cost,
  predicted_values$sum_burden - observed_values$sum_burden
)

# Perform t-tests (assuming paired t-test for simplicity)
t_tests <- list(
  t.test(tax_returns_df_18$NPV_revenue_cate[tax_returns_df_18$audited_current == 1], 
         tax_returns_df_18$NPV_taxrevenue_current[tax_returns_df_18$audited_current == 1], paired = TRUE),
  t.test(tax_returns_df_18$NPV_revenue_cate[tax_returns_df_18$audited_current == 1], 
         tax_returns_df_18$NPV_taxrevenue_current[tax_returns_df_18$audited_current == 1], paired = TRUE),
  t.test(tax_returns_df_18$audit_cost_cate[tax_returns_df_18$audited_current == 1], 
         tax_returns_df_18$audit_cost_current[tax_returns_df_18$audited_current == 1], paired = TRUE),
  t.test(tax_returns_df_18$burden_cate[tax_returns_df_18$audited_current == 1], 
         tax_returns_df_18$burden_current[tax_returns_df_18$audited_current == 1], paired = TRUE)
)

# Extract p-values from t-tests
p_values <- sapply(t_tests, function(test) round(test$p.value, 2))

# Create a data frame for the table
results_df <- data.frame(
  Metric = c("MVPF", "Sum of Revenue", "Sum of Cost", "Sum of Burden"),
  Observed = c(observed_values$MVPF, observed_values$sum_revenue, observed_values$sum_cost, observed_values$sum_burden),
  `GRF Predicted` = c(predicted_values$MVPF, predicted_values$sum_revenue, predicted_values$sum_cost, predicted_values$sum_burden),
  Difference = differences,
  `p-value` = p_values
)

# Create the LaTeX table
latex_table <- kable(results_df, 
                     caption = "Comparison of Observed and GRF Predicted Values", 
                     format = "latex", booktabs = TRUE, row.names = FALSE,
                     label = "grf_predict_observed") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"))


# Save the LaTeX table
writeLines(latex_table, file.path(figures_output_path, 
                                  "/tables/GRF_predict_MVPF_observed.tex"))


# Distributions of treatment effects --------------------------

ATEs <- average_treatment_effect(cf_model_full_17)

# distribution of NPV revenue predictions:
plot <- ggplot(cate_hats_2017, aes(x = NPV_revenue_cate)) +
  geom_histogram(binwidth = 100000, color = "black", alpha = 0.7) +  
  labs(title = "Distribution of Estimated Treatment Effects: R", 
       x = "Treatment Effect: R", 
       y = "Frequency") +
  geom_vline(aes(xintercept = ATEs[1,1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs[1,1], y = 4, label = paste("ATE =", round(ATEs[1,1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(figures_output_path, "cate_dist_R.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# distribution of audit cost predictions:
plot <- ggplot(cate_hats_2017, aes(x = audit_cost_cate)) +
  geom_histogram(binwidth = 100, color = "black", alpha = 0.7) +  
  labs(title = "Distribution of Estimated Treatment Effects: C", 
       x = "Treatment Effect: C", 
       y = "Frequency") +
  geom_vline(aes(xintercept = ATEs[2,1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs[2,1], y = 4, label = paste("ATE =", round(ATEs[2,1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(figures_output_path, "cate_dist_C.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# distribution of burden predictions:
plot <- ggplot(cate_hats_2017, aes(x = burden_cate)) +
  geom_histogram(binwidth = 100, color = "black", alpha = 0.7) +  
  labs(title = "Distribution of Estimated Treatment Effects: B", 
       x = "Treatment Effect: B", 
       y = "Frequency") +
  geom_vline(aes(xintercept = ATEs[3,1]), color = "red", linetype = "dashed", size = 1) + 
  geom_text(aes(x = ATEs[3,1], y = 4, label = paste("ATE =", round(ATEs[3,1], 2))), 
            vjust = -0.5, hjust = 1.2, color = "red", size = 5) +
  theme_minimal()

ggsave(filename = file.path(figures_output_path, "cate_dist_B.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)


# VIF scores -------------------------------

vif_scores <- variable_importance(cf_model_full_17)
top_15_indices <- order(vif_scores, decreasing = TRUE)[1:15]
top_15_scores <- vif_scores[top_15_indices]

# Custom variable names for the top 15 variables (modify as needed)
custom_names <- c("Var1", "Var2", "Var3", "Var4", "Var5", 
                  "Var6", "Var7", "Var8", "Var9", "Var10", 
                  "Var11", "Var12", "Var13", "Var14", "Var15")

# Create a dataframe with the custom names and their corresponding VIF scores
vif_df <- data.frame(Variable = custom_names, VIF_Score = top_15_scores)

# Plot the variable importance as a horizontal bar chart
plot <- ggplot(vif_df, aes(x = reorder(Variable, VIF_Score), y = VIF_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +    
  coord_flip() +                                       
  labs(title = "Variable Importance (VIF Scores)", 
       x = "Variable", 
       y = "Variable Importance Score") +
  theme_minimal()

ggsave(filename = file.path(figures_output_path, "vif_scores.png"), plot = plot, 
       width = 8, height = 6, dpi = 300)

# Rank Average Treatment Effect ------------------------

cate_hats_test <- predict(
  object = cf_model_test, 
  X_matrix_train[test, ],
  drop = TRUE,
  estimate.variance = FALSE)$predictions

cate_hats_2018 <- as.data.frame(
  predict(
    object = cf_model_full_17,
    newdata = tax_returns_df_18[, X_covariates],
    drop = TRUE, # removes the unnecessary dimension for multiple treat arms
    estimate.variance = FALSE
  )$predictions)
colnames(cate_hats_2018) <- c("NPV_revenue_cate", "audit_cost_cate", "burden_cate")

rate <- rank_average_treatment_effect(cf_model_eval, 
                                      cate_hats_test,
                                      target = "AUTOC")

png(file.path(figures_output_path, "rate_revenue.png"))
plot(rate, xlab = "Treated Fraction", 
     main = "TOC evaluated on hold-out\n tau(X) estimated from test forest",
     ylab = "ATE Gain")
text(x = 0.8, y = max(rate$estimate), 
     labels = paste("AUTOC:", round(rate$estimate, 2)), 
     pos = 4, cex = 1.2, col = "blue")
dev.off()





# export data for optimization ---------------------------

write.csv(tax_returns_df_18, file.path(data_path, "tax_returns_post_ML.csv"), row.names = FALSE)

cat("All Forests Trained/Saved and Predictions are Saved \n")


















