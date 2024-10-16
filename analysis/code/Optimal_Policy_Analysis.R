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
library(knitr)
library(kableExtra)
library(VennDiagram)

data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output/tables"
figures_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output"

# Import 2013-2021 tax returns 
tax_returns_df <- read.csv(file.path(data_path, "tax_returns_post_opt.csv"))

#generating a random "observed" treatment sequence (DELETE ONCE THE DATASET IS REAL)
tax_returns_df$audited_current <- sample(c(0, 1), size = nrow(tax_returns_df), 
                                         replace = TRUE, prob = c(0.9, 0.1))


# Create table calculating MVPF, Revenue, and Cost comparisons across policies -------------------

# Function to calculate MVPF, cost, revenue, and num treated under each policy
calculate_metrics <- function(df, W_col) {
  MVPF <- sum(df[[W_col]] * (df$R + df$B)) / sum(df[[W_col]] * (df$R - df$C))
  R <- sum(df[[W_col]] * df$R)
  C <- sum(df[[W_col]] * df$C)
  N <- sum(df[[W_col]])
  return(c(MVPF = MVPF, R = R, C = C, N = N))
}

# List of W_opt columns for each panel
W_opt_cols <- grep("^W_opt_", colnames(tax_returns_df), value = TRUE)

# Loop through each panel and calculate metrics
results_list <- list()

for (panel in c("1a", "1b", "2a", "2b")) {
  # Extract relevant columns for the panel
  panel_cols <- grep(paste0("^W_opt_", panel), W_opt_cols, value = TRUE)
  
  # Initialize the data frame for this panel
  panel_df <- data.frame(
    Metric = c("MVPF", "Total Revenue", "Government Expenditure", "Number Treated"),
    `Observed Policy` = round(calculate_metrics(tax_returns_df, "audited_current"), 2)
  )
  
  # Add each W_opt column to the panel
  for (col in panel_cols) {
    alpha_val <- gsub(paste0("^W_opt_", panel, "_"), "", col)
    panel_df[[paste0("alpha = ", alpha_val)]] <- round(calculate_metrics(tax_returns_df, col), 2)
  }
  
  results_list[[panel]] <- panel_df
}

# Define captions for each panel
captions <- list(
  "1a" = "Policy 1a: MVPF Minimization with Revenue Constraint",
  "1b" = "Policy 1b: MVPF Minimization with Revenue and Cost Constraint",
  "2a" = "Policy 2a: Revenue Maximization with MVPF Constraint",
  "2b" = "Policy 2b: Revenue Maximization with MVPF and Cost Constraint"
)

# Define labels for each panel
labels <- list(
  "1a" = "optimal_vs_observed_policy_1a",
  "1b" = "optimal_vs_observed_policy_1b",
  "2a" = "optimal_vs_observed_policy_2a",
  "2b" = "optimal_vs_observed_policy_2b"
)

# Create LaTeX tables for each panel
latex_tables <- list()

for (panel in c("1a", "1b", "2a", "2b")) {
  panel_table <- kable(results_list[[panel]], 
                       caption = captions[[panel]], 
                       format = "latex", booktabs = TRUE, row.names = FALSE,
                       label = labels[[panel]]) %>%
    kable_styling(latex_options = c("striped", "scale_down", "HOLD_position")) %>%
    add_header_above(c(" " = 1, "Redistribution Preferences" = length(results_list[[panel]]) - 1))
  
  latex_tables[[panel]] <- panel_table
}

# Combine all tables into a single LaTeX table
combined_latex_table <- paste(latex_tables, collapse = "\n\n")

# Save the LaTeX table
writeLines(combined_latex_table, file.path(output_path, "MVPF_comparison_table.tex"))

# Create Table for Covariate Mean Comparison between treated groups ------------------------

# Function to calculate group covariate means
calculate_group_means <- function(df, covariate, W_col) {
  return(mean(df[[covariate]] * df[[W_col]], na.rm = TRUE))
}

# Function to calculate the difference in group covariate means and perform t-test
calculate_diff_means <- function(df, covariate, W_col, observed_mean) {
  policy_mean <- mean(df[[covariate]] * df[[W_col]], na.rm = TRUE)
  diff_mean <- policy_mean - observed_mean
  t_test <- t.test(df[[covariate]] * df[[W_col]], df[[covariate]] * df[["audited_current"]])
  p_value <- t_test$p.value
  stars <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
  return(paste0(round(diff_mean, 2), stars))
}

# List of W_opt columns for each panel
W_opt_cols <- grep("^W_opt_", colnames(tax_returns_df), value = TRUE)

# List of covariates to calculate means for
covariates <- c("income")

# Loop through each panel and calculate metrics
results_list <- list()

for (panel in c("1a", "1b", "2a", "2b")) {
  # Extract relevant columns for the panel
  panel_cols <- grep(paste0("^W_opt_", panel), W_opt_cols, value = TRUE)
  
  # Initialize the data frame for this panel
  panel_df <- data.frame(
    Covariate = covariates,
    `Covariate Mean` = sapply(covariates, function(cov) calculate_group_means(tax_returns_df, cov, "audited_current"))
  )
  
  # Add each W_opt column to the panel
  for (col in panel_cols) {
    alpha_val <- gsub(paste0("^W_opt_", panel, "_"), "", col)
    panel_df[[paste0("alpha = ", alpha_val)]] <- sapply(covariates, function(cov) {
      observed_mean <- calculate_group_means(tax_returns_df, cov, "audited_current")
      calculate_diff_means(tax_returns_df, cov, col, observed_mean)
    })
  }
  
  results_list[[panel]] <- panel_df
}

# Define captions for each panel
captions <- list(
  "1a" = "Policy 1a: Group Covariate Means",
  "1b" = "Policy 1b: Group Covariate Means",
  "2a" = "Policy 2a: Group Covariate Means",
  "2b" = "Policy 2b: Group Covariate Means"
)

# Define labels for each panel
labels <- list(
  "1a" = "cov_means_policy_1a",
  "1b" = "cov_means_policy_1b",
  "2a" = "cov_means_policy_2a",
  "2b" = "cov_means_policy_2b"
)

# Create LaTeX tables for each panel
latex_tables <- list()

for (panel in c("1a", "1b", "2a", "2b")) {
  panel_table <- kable(results_list[[panel]], 
                       caption = captions[[panel]], 
                       format = "latex", booktabs = TRUE, row.names = FALSE, 
                       label = labels[[panel]]) %>%
    kable_styling(latex_options = c("striped", "scale_down", "HOLD_position")) %>%
    add_header_above(c(" " = 1, "Observed Policy" = 1, "Optimal Policy - Observed Policy" = length(panel_cols)))
  
  latex_tables[[panel]] <- panel_table
}

# Combine all tables into a single LaTeX table
combined_latex_table <- paste(latex_tables, collapse = "\n\n")

# Save the LaTeX table
writeLines(combined_latex_table, file.path(output_path, "Group_Covariate_Means_Table.tex"))

# Create Venn Diagram for MVPFs of overlap policies ------------------------

# Function to calculate MVPFs
calculate_mvpfs <- function(df, W_col, audited_col) {
  df_filtered <- df %>% filter(df[[audited_col]] == 1)
  MVPF <- sum(df_filtered[[W_col]] * (df_filtered$R + df_filtered$B)) / sum(df_filtered[[W_col]] * (df_filtered$R - df_filtered$C))
  return(MVPF)
}

# Function to calculate MVPF for the entire subset
calculate_mvpfs_entire <- function(df, W_col) {
  MVPF <- sum(df[[W_col]] * (df$R + df$B)) / sum(df[[W_col]] * (df$R - df$C))
  return(MVPF)
}

# List of W_opt columns for each panel
W_opt_cols <- grep("^W_opt_", colnames(tax_returns_df), value = TRUE)

# Loop through each panel and create Venn diagrams
for (panel in c("1a", "1b", "2a", "2b")) {
  # Extract relevant columns for the panel
  panel_cols <- grep(paste0("^W_opt_", panel), W_opt_cols, value = TRUE)
  
  for (col in panel_cols) {
    alpha_val <- gsub(paste0("^W_opt_", panel, "_"), "", col)
    
    # Calculate MVPFs for each region
    observed_mvpfs <- round(calculate_mvpfs(tax_returns_df, "audited_current", "audited_current"), 2)
    optimal_mvpfs <- round(calculate_mvpfs(tax_returns_df, col, col), 2)
    overlap_mvpfs <- round(calculate_mvpfs(tax_returns_df, col, "audited_current"), 2)
    
    # Make sure overlap does not exceed either area
    overlap_mvpfs <- min(overlap_mvpfs, observed_mvpfs, optimal_mvpfs)
    
    # Calculate MVPFs for the entire subsets
    observed_mvpfs_entire <- calculate_mvpfs_entire(tax_returns_df, "audited_current")
    optimal_mvpfs_entire <- calculate_mvpfs_entire(tax_returns_df, col)
    
    # Create Venn diagram with larger font sizes for labels and numbers
    venn.plot <- draw.pairwise.venn(
      area1 = observed_mvpfs,
      area2 = optimal_mvpfs,
      cross.area = overlap_mvpfs,
      category = c("Observed Policy", paste0("Optimal Policy")),
      fill = c("blue", "red"),
      alpha = 0.5,
      cat.pos = c(-20, 20),
      cat.dist = c(0.05, 0.05),
      scaled = TRUE,
      cat.cex = 2.5,  
      cex = 2.5 
    )
    
    # Save the Venn diagram
    venn_file <- file.path(figures_output_path, paste0("Venn_Diagram_", panel, "_alpha_", alpha_val, ".png"))
    
    # Open the png device
    png(venn_file, width = 800, height = 600)  # Adjust size as needed
    
    # Plot the Venn diagram
    grid.draw(venn.plot)
    
    # Add MVPFs for the entire subsets above the circles (rounded to 2 decimal places, with larger font)
    grid.text(paste("MVPF (Observed Policy):", round(observed_mvpfs_entire, 2)), 
              x = 0.3, y = 0.9, gp = gpar(fontsize = 24, col = "blue"))  
    grid.text(paste("MVPF (Optimal Policy):", round(optimal_mvpfs_entire, 2)), 
              x = 0.7, y = 0.9, gp = gpar(fontsize = 24, col = "red"))  
    
    # Close the png device to save the file
    dev.off()
  }
}








