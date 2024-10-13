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
figures_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output"
cf_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Large_Output"

# Import 2013-2021 tax returns 
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_data_ML_cleaned.csv"))

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
  
  # Select columns that include the current year, the 3 years before, and the 3 years after, always keep 'ntn'
  selected_columns <- tax_returns_df %>%
    select(
      ntn,
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
  
  rm(renamed_df)
  
  # Print confirmation
  cat("Created dataset:", df_name, "\n")
}

# Estimate full multi-arm causal forest --------------------------

X_covariates <- c("taxableinc_b3", "taxableinc_b2", "taxableinc_b1", "taxableinc_current",
                  "incfrsalary_b3", "incfrsalary_b2", "incfrsalary_b1", "incfrsalary_current")

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


# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_2017_model <- FALSE # 2017 data is from 2016 tax returns

if (train_2017_model) {
  
  # Estimate the causal forest model
  cf_model_full_17 <- multi_arm_causal_forest(X = X_matrix_train,
                                              Y = Y_matrix_train,
                                              W = W_vector_train,
                                              Y.hat = NULL,
                                              W.hat = NULL,
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
  
} else {
  
  # Load the previously saved model
  cf_model_full_17 <- readRDS(file.path(cf_output_path, "cf_model_full_trained_2017.rda"))
}

# Estimate evaluation forests for prediction quality tests -----------------------------

X_covariates <- c("taxableinc_b3", "taxableinc_b2", "taxableinc_b1", "taxableinc_current",
                  "incfrsalary_b3", "incfrsalary_b2", "incfrsalary_b1", "incfrsalary_current")

# need to define the training objects as matrices/vectors
X_matrix_train <- as.matrix(tax_returns_df_17[, X_covariates])
# only using revenue for RATE because multi-outcomes don't work
Y_matrix_train <- as.vector(tax_returns_df_17$NPV_taxrevenue_current)
W_vector_train <- as.vector(tax_returns_df_17$audited_current)


# Toggle to TRUE if you want to re-train the causal forest, if FALSE it will load the saved model
train_2017_test_models <- TRUE # 2017 data is from 2016 tax returns

if (train_2017_test_models) {
  
  n <- nrow(X_matrix_train)
  train <- sample(1:n, n * 0.7) # training on 70% of the data
  test <- setdiff(1:n, train) # evaluate on remaining 30%
  
  # Estimate the causal forest test and evaluation models
  cf_model_test <- causal_forest(X = X_matrix_train[train, ],
                                 Y = Y_matrix_train[train],
                                 W = W_vector_train[train],
                                 Y.hat = NULL,
                                 W.hat = NULL,
                                 num.trees = 1000,
                                 honesty = TRUE,
                                 honesty.fraction = 0.5,
                                 honesty.prune.leaves = TRUE,
                                 alpha = 0.05,
                                 imbalance.penalty = 0,
                                 stabilize.splits = TRUE,
                                 min.node.size = 5
                                 )
  
  cf_model_eval <- causal_forest(X = X_matrix_train[test, ],
                                 Y = Y_matrix_train[test],
                                 W = W_vector_train[test],
                                 Y.hat = NULL,
                                 W.hat = NULL,
                                 num.trees = 1000,
                                 honesty = TRUE,
                                 honesty.fraction = 0.5,
                                 honesty.prune.leaves = TRUE,
                                 alpha = 0.05,
                                 imbalance.penalty = 0,
                                 stabilize.splits = TRUE,
                                 min.node.size = 5
                                 )
  
  
  # Save the causal forest models
  saveRDS(cf_model_test, file = paste0(cf_output_path, "/cf_model_test.rda"))
  saveRDS(cf_model_eval, file = paste0(cf_output_path, "/cf_model_evaluation.rda"))
  
} else {
  
  # Load the previously saved models
  cf_model_test <- readRDS(file.path(cf_output_path, "cf_model_test.rda"))
  cf_model_eval <- readRDS(file.path(cf_output_path, "cf_model_eval.rda"))
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
rate

png(file.path(figures_output_path, "rate_revenue.png"))
plot(rate, xlab = "Treated Fraction", 
     main = "TOC evaluated on hold-out\n tau(X) estimated from test forest",
     ylab = "ATE Gain")
dev.off()





# export data for optimization ---------------------------

write.csv(tax_returns_df_18, file.path(data_path, "tax_returns_post_ML.csv"), row.names = FALSE)




















