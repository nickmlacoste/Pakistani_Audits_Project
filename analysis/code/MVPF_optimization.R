# Intro -----------------------------

"
This script performs optimization. We consider 4 policies:
1. Minimize MVPF such that revenue >= observed revenue
2. Miniimze MVPF such that revenue >= observed revenue and cost <= observed cost
3. Maximize revenue such that MVPF <= observed MVPF
4. Maximize revenue such that MVPF <= observed MVPF and cost <= observed cost

We use a form of mini-batch stochastic gradient descent for problems 1 and 2. For problems 
3 and 4 we employ a genetic algorthm
"

rm(list = ls())

library(tidyverse)
library(GA)
library(plotrix)


data_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Data"
figures_output_path <- "C:/Users/nickm/OneDrive/Acer (new laptop)/Documents/PhD/Tulane University/Projects/Pakistan Audits/Pakistani_Audits_Project/analysis/output"

# Import data with GRF predictions
df <- read.csv(file.path(data_path, "tax_returns_post_ML.csv"))
df <- df %>%
  rename(
    R = NPV_revenue_cate,
    B = burden_cate,
    C = audit_cost_cate
  )

##### placeholder data generation function - remove once you have real CATE estimates ####

generate_dataset <- function(n) {
  set.seed(42)  # For reproducibility
  R <- runif(n, min = 5, max = 20)  # Revenue (R_i)
  C <- runif(n, min = 1, max = 10)  # Cost (C_i)
  B <- runif(n, min = 1, max = 10)  # Burden (B_i)
  income <- runif(n, min = 1, max = 500000)
  
  # Calculate individual MVPF
  indiv_MVPF <- (R + B) / (R - C)
  
  # Return the dataset as a data frame
  return(data.frame(R, B, C, income, indiv_MVPF))
}

df <- generate_dataset(n=1000)
####################################################################################


###################################################
############# Minimize MVPF problems ##############
###################################################

# Define Mini-Batch Stochastic Gradient Descent algorithm -----------------------

# Function to scale R and B based on income and alpha
scale_R_B <- function(R, B, income, alpha, mean_income) {
  R_scaled <- (income^alpha / mean_income^alpha) * R
  B_scaled <- (mean_income^alpha / income^alpha) * B
  return(list(R_scaled = R_scaled, B_scaled = B_scaled))
}

# Calculate the mean income
mean_income <- mean(df$income)

# Define the objective function
objective_function <- function(R, B, C, treatment) {
  numerator <- sum(treatment * (R + B))
  denominator <- sum(treatment * (R - C))
  return(numerator / denominator)
}

# Gradient of the objective function with respect to the treatment for each individual
gradient <- function(R, B, C, treatment, i) {
  numerator_1 <- (R[i] + B[i]) * sum(treatment * (R - C)) 
  numerator_2 <- (R[i] - C[i]) * sum(treatment * (R + B))
  return((numerator_1 - numerator_2) / sum(treatment * (R - C))^2)
}

# Initialize treatment with optional cost constraint
initialize_treatment_with_constraints <- function(df, bar_R, bar_C, cost_constraint) {
  # Sort individuals by R_i in descending order
  sorted_indices <- order(df$R, decreasing = TRUE)
  
  # Initialize treatment to zeros (no one treated)
  treatment <- rep(0, nrow(df))
  
  # Greedily assign treatment starting from the highest R_i
  total_R <- 0
  total_C <- 0
  for (i in sorted_indices) {
    if (total_R < bar_R && (!cost_constraint || (total_C + df$C[i]) <= bar_C)) {
      treatment[i] <- 1  # Assign treatment
      total_R <- total_R + df$R[i]
      total_C <- total_C + df$C[i]
    }
    if (total_R >= bar_R && (!cost_constraint || total_C <= bar_C)) break  # Stop when both constraints are satisfied
  }
  
  # Randomly assign treatment to the remaining individuals while respecting \bar{C}
  remaining_indices <- setdiff(1:nrow(df), which(treatment == 1))
  for (i in remaining_indices) {
    if (!cost_constraint || (total_C + df$C[i]) <= bar_C) {
      treatment[i] <- sample(0:1, 1)  # Randomly assign 0 or 1
      if (treatment[i] == 1) total_C <- total_C + df$C[i]
    }
  }
  
  return(treatment)
}

# Mini-batch SGD with optional cost constraint
mini_batch_sgd_cost <- function(df, bar_R, bar_C, batch_size, max_iter, cost_constraint) {
  
  # Initialize treatment with both constraints
  treatment <- initialize_treatment_with_constraints(df, bar_R, bar_C, cost_constraint)
  
  # Store objective function values
  obj_values <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    # Randomly select a mini-batch
    batch_indices <- sample(1:nrow(df), batch_size)
    
    # For each individual in the mini-batch, calculate the gradient and update treatment
    for (i in batch_indices) {
      grad <- gradient(df$R, df$B, df$C, treatment, i)
      
      # Update treatment based on gradient
      if (grad > 0) {
        treatment[i] <- 0  # Turn off treatment if gradient is positive
      } else {
        treatment[i] <- 1  # Turn on treatment if gradient is negative
      }
    }
    
    # Ensure that the constraint \sum_i R_i >= \bar{R} is satisfied
    if (sum(treatment * df$R) < bar_R) {
      top_r_index <- order(df$R, decreasing = TRUE)
      for (j in top_r_index) {
        if (treatment[j] == 0) {
          treatment[j] <- 1
          if (sum(treatment * df$R) >= bar_R) break
        }
      }
    }
    
    # Ensure that the constraint \sum_i C_i <= \bar{C} is satisfied (if cost_constraint is TRUE)
    if (cost_constraint && sum(treatment * df$C) > bar_C) {
      top_c_index <- order(df$C, decreasing = TRUE)
      for (j in top_c_index) {
        if (treatment[j] == 1) {
          treatment[j] <- 0
          if (sum(treatment * df$C) <= bar_C) break
        }
      }
    }
    
    # Calculate and store the value of the objective function
    obj_values[iter] <- objective_function(df$R, df$B, df$C, treatment)
  }
  
  # Calculate final metrics
  total_revenue <- sum(treatment * df$R)
  total_cost <- sum(treatment * df$C)
  num_treated <- sum(treatment)
  final_obj_value <- tail(obj_values, 1)
  
  # Print out the final results
  cat("Final treatment allocation (total number treated):", num_treated, "\n")
  cat("Total Preference-weighted Revenue (sum R):", total_revenue, "\n")
  cat("Total Cost (sum C):", total_cost, "\n")
  cat("Final MVPF Value:", final_obj_value, "\n")
  
  # Return the final treatment allocation, objective function values, and metrics
  return(list(treatment = treatment, obj_values = obj_values, 
              total_revenue = total_revenue, total_cost = total_cost, 
              num_treated = num_treated, final_obj_value = final_obj_value))
}

# Loop through alpha values and optimize
optimize_for_alpha <- function(df, alpha_values, bar_R, bar_C, batch_size, max_iter, cost_constraint) {
  mean_income <- mean(df$income)
  
  for (alpha in alpha_values) {
    cat("Optimizing for alpha =", alpha, "\n")
    
    # Scale R and B using the current value of alpha
    scaled_values <- scale_R_B(df$R, df$B, df$income, alpha, mean_income)
    df$R <- scaled_values$R_scaled
    df$B <- scaled_values$B_scaled
    
    # Run the mini-batch SGD with the given constraints
    result_mbsgd <- mini_batch_sgd_cost(df, bar_R, bar_C, batch_size, max_iter = max_iter, cost_constraint = cost_constraint)
    
    # Dynamic plot naming based on if we enforce the cost constraint
    if (cost_constraint) {
      plot_file <- paste0("SGD_convergence_1b_", gsub("\\.", "", as.character(alpha)), ".png")
      col_name <- paste0("W_opt_1b_", gsub("\\.", "", as.character(alpha)))
    } else {
      plot_file <- paste0("SGD_convergence_1a_", gsub("\\.", "", as.character(alpha)), ".png")
      col_name <- paste0("W_opt_1a_", gsub("\\.", "", as.character(alpha)))
    }
    
    # Save the plot
    png(filename = file.path(figures_output_path, plot_file), width = 800, height = 600)
    
    plot(result_mbsgd$obj_values, type = "l", col = "red", 
         main = paste("MVPF Convergence (Alpha =", alpha, ")"), 
         xlab = "Iteration", ylab = "MVPF Value")
    
    dev.off()
    
    # Append the optimal policy as a column to df
    df[[col_name]] <- result_mbsgd$treatment
    
  }
  
  return(df)
  
}

# Policy 1a: MVPF minimization, minimum revenue constraint ---------------------

alpha_values <- c(0, 0.2, 0.5)
df <- optimize_for_alpha(df, alpha_values, 
                         bar_R = 5000, bar_C = NULL, 
                         batch_size = 64, max_iter = 2000, 
                         cost_constraint = FALSE)

write.csv(df, file.path(data_path, "tax_returns_post_opt.csv"), row.names = FALSE)

# Policy 1b: MVPF minimization, minimum revenue and maximum cost constraint ----------------

alpha_values <- c(0, 0.2, 0.5)
df <- optimize_for_alpha(df, alpha_values, 
                         bar_R = 5000, bar_C = 1000, 
                         batch_size = 64, max_iter = 2000, 
                         cost_constraint = TRUE)

write.csv(df, file.path(data_path, "tax_returns_post_opt.csv"), row.names = FALSE)


################################################
##### Revenue maximization problems ############
################################################

# Define Genetic Algorithm ---------------------------------

# Define the MVPF constraint
mvpf_constraint <- function(W, R, B, C, MVPF_max) {
  numerator <- sum(W * (R + B))
  denominator <- sum(W * (R - C))
  
  if (denominator > 0) {
    mvpf <- numerator / denominator
    return(mvpf)
  } else {
    return(Inf)  # If denominator is non-positive, constraint is violated
  }
}

check_mvpf_constraint <- function(W, R, B, C, MVPF_max) {
  numerator <- sum(W * (R + B))
  denominator <- sum(W * (R - C))
  
  if (denominator > 0) {
    mvpf <- numerator / denominator
    return(mvpf <= MVPF_max)  # Return TRUE if MVPF is valid
  } else {
    return(FALSE)  # Return FALSE if the MVPF is invalid
  }
}

# Define evaluation function with optional cost constraint
evaluation_function <- function(W, R, B, C, MVPF_max, Cost_max, cost_constraint) {
  mvpf <- mvpf_constraint(W, R, B, C, MVPF_max)
  total_cost <- sum(W * C)  # Calculate total cost for treated individuals
  
  # Check if MVPF and (optionally) cost constraints are satisfied
  if (mvpf <= MVPF_max && (!cost_constraint || total_cost <= Cost_max)) {
    total_revenue <- sum(W * R)  # Calculate total revenue if constraints are met
  } else {
    # Apply a penalty if the MVPF is violated, or the cost constraint if cost_constraint = TRUE
    penalty <- 10000000 * (max(0, mvpf - MVPF_max) + if (cost_constraint) max(0, total_cost - Cost_max) else 0)
    total_revenue <- sum(W * R) - penalty  # Apply penalty to revenue
  }
  
  # Store the total revenue and MVPF for this iteration
  revenue_history <<- c(revenue_history, sum(W * R))
  mvpf_history <<- c(mvpf_history, mvpf)
  
  return(total_revenue)
}

# Modify the population initialization function to handle optional cost constraint
initialize_population_valid_fast <- function(object, R, B, C, MVPF_max, Cost_max, individual_MVPF, cost_constraint) {
  # Create a population where only valid individuals are included
  n <- object@nBits
  pop <- matrix(0, nrow = object@popSize, ncol = n)
  
  # Filter individuals with individual MVPF <= MVPF_max
  valid_indices <- which(individual_MVPF <= MVPF_max)
  valid_pool_size <- length(valid_indices)
  
  if (valid_pool_size == 0) {
    stop("No individuals with valid MVPF found.")
  }
  
  # Generate population, ensuring valid MVPF and (optionally) cost constraint for each individual
  for (i in 1:object@popSize) {
    valid <- FALSE
    while (!valid) {
      # Randomly treat 10 individuals from the valid pool
      W <- rep(0, n)
      num_treated <- min(1, valid_pool_size)  # Treat at most 10 valid individuals
      treated_indices <- sample(valid_indices, num_treated)
      W[treated_indices] <- 1
      
      # Check if cumulative MVPF and (optionally) cost are valid
      valid <- check_mvpf_constraint(W, R, B, C, MVPF_max) && (!cost_constraint || sum(W * C) <= Cost_max)
    }
    pop[i, ] <- W  # Store valid individual in population
  }
  
  return(pop)
}


# Policy 2a: Revenue max, MVPF constraint, no cost constraint ------------------

# Maximum allowable cost and MVPF
MVPF_max <- 1.5
Cost_max <- NULL

# Set to TRUE to enforce the cost constraint, FALSE to ignore it
cost_constraint <- FALSE  

# Define the list of concavity parameters to optimize on
alpha_values <- c(0, 0.2, 0.5)

# Function to scale R and B based on income and alpha
scale_R_B <- function(R, B, income, alpha, mean_income) {
  R_scaled <- (income^alpha / mean_income^alpha) * R
  B_scaled <- (mean_income^alpha / income^alpha) * B
  return(list(R_scaled = R_scaled, B_scaled = B_scaled))
}

# Calculate the mean income once
mean_income <- mean(df$income)

for (alpha in alpha_values) {
  cat("Running GA for alpha =", alpha, "\n")
  
  # Scale R and B using the current value of alpha
  scaled_values <- scale_R_B(df$R, df$B, df$income, alpha, mean_income)
  R_scaled <- scaled_values$R_scaled
  B_scaled <- scaled_values$B_scaled
  
  # vectors to store revenues and MVPFs across iterations
  revenue_history <- c()
  mvpf_history <- c()
  
  # Run the GA with the scaled R and B values
  ga_result <- ga(
    type = "binary",
    fitness = function(W) evaluation_function(W, R_scaled, B_scaled, df$C, MVPF_max, Cost_max, cost_constraint),
    nBits = nrow(df),   # Number of bits equal to number of individuals
    popSize = 100,      # Population size
    maxiter = 1000,     # Maximum number of generations
    run = 50,           # Stop if no improvement for 50 generations
    pmutation = 0.2,    # Higher mutation rate to allow for exploration
    population = function(object) initialize_population_valid_fast(object, 
                                                                   R_scaled, B_scaled, df$C, 
                                                                   MVPF_max, Cost_max, 
                                                                   df$indiv_MVPF, 
                                                                   cost_constraint),  # Initialize valid population
    seed = 42           # Seed for reproducibility
  )
  
  # Print the results of the optimal policy for this alpha
  cat("Maximized revenue for alpha =", alpha, ":", sum(ga_result@solution * R_scaled), "\n")
  
  # Check the MVPF of the final solution
  final_W <- ga_result@solution
  final_numerator <- sum(final_W * (R_scaled + B_scaled))
  final_denominator <- sum(final_W * (R_scaled - df$C))
  final_mvpf <- final_numerator / final_denominator
  
  # Calculate the total cost for the optimal policy
  total_cost <- sum(final_W * df$C)
  
  # Print the final MVPF and total cost
  cat("Final MVPF for alpha =", alpha, ":", final_mvpf, "\n")
  cat("Total cost of the optimal policy for alpha =", alpha, ":", total_cost, "\n")
  
  # Save the optimal allocation to the original dataframe
  column_name <- paste0("opt_2a_a", gsub("\\.", "", as.character(alpha)))
  final_W_df <- data.frame(t(final_W))
  names(final_W_df) <- column_name
  df[[column_name]] <- final_W_df[[column_name]]
  
  
  # Create a data frame for plotting convergence
  iterations <- seq_along(revenue_history)
  plot_data <- data.frame(
    Iteration = iterations,
    Total_Revenue = revenue_history,
    Aggregate_MVPF = mvpf_history
  )
  plot_data <- plot_data %>%
    mutate(Cumulative_Max_Revenue = cummax(Total_Revenue),
           Corresponding_MVPF = Aggregate_MVPF[match(Cumulative_Max_Revenue, Total_Revenue)])
  
  # Set the figure file name based on the alpha value
  figure_file <- paste0("GA_convergence_2a_a", gsub("\\.", "", as.character(alpha)), ".png")
  
  # Save the plot as a PNG file
  png(filename = file.path(figures_output_path, figure_file), width = 800, height = 600)
  
  par(mar = c(5, 4, 4, 5))  # Set margins for the plot
  
  # Generate the plot using twoord.plot
  twoord.plot(
    lx = plot_data$Iteration, ly = plot_data$Cumulative_Max_Revenue,
    rx = plot_data$Iteration, ry = plot_data$Corresponding_MVPF,
    lylim = range(plot_data$Cumulative_Max_Revenue, na.rm = TRUE),
    rylim = range(plot_data$Corresponding_MVPF, na.rm = TRUE),
    lcol = "blue", rcol = "red",
    xlab = "Iteration", ylab = "Revenue of Best Policy", rylab = "MVPF of Best Policy",
    main = paste("Genetic Algorithm Policy Convergence for alpha =", alpha),
    type = c("l", "l"),
    do.first = "grid()"
  )
  
  # Add axis labels
  axis(side = 2, at = pretty(plot_data$Cumulative_Max_Revenue), las = 0, col.axis = "blue", col = "blue")
  
  # Finalize and save the plot
  dev.off()
  
}



# Policy 2b: Revenue max, MVPF and cost constraints -----------------------

# Maximum allowable cost and MVPF
MVPF_max <- 1.5
Cost_max <- 200

# Set to TRUE to enforce the cost constraint, FALSE to ignore it
cost_constraint <- TRUE  

# Define the list of concavity parameters to optimize on
alpha_values <- c(0, 0.2, 0.5)

# Function to scale R and B based on income and alpha
scale_R_B <- function(R, B, income, alpha, mean_income) {
  R_scaled <- (income^alpha / mean_income^alpha) * R
  B_scaled <- (mean_income^alpha / income^alpha) * B
  return(list(R_scaled = R_scaled, B_scaled = B_scaled))
}

# Calculate the mean income once
mean_income <- mean(df$income)

for (alpha in alpha_values) {
  cat("Running GA for alpha =", alpha, "\n")
  
  # Scale R and B using the current value of alpha
  scaled_values <- scale_R_B(df$R, df$B, df$income, alpha, mean_income)
  R_scaled <- scaled_values$R_scaled
  B_scaled <- scaled_values$B_scaled
  
  # vectors to store revenues and MVPFs across iterations
  revenue_history <- c()
  mvpf_history <- c()
  
  # Run the GA with the scaled R and B values
  ga_result <- ga(
    type = "binary",
    fitness = function(W) evaluation_function(W, R_scaled, B_scaled, df$C, MVPF_max, Cost_max, cost_constraint),
    nBits = nrow(df),   # Number of bits equal to number of individuals
    popSize = 100,      # Population size
    maxiter = 1000,     # Maximum number of generations
    run = 50,           # Stop if no improvement for 50 generations
    pmutation = 0.2,    # Higher mutation rate to allow for exploration
    population = function(object) initialize_population_valid_fast(object, 
                                                                   R_scaled, B_scaled, df$C, 
                                                                   MVPF_max, Cost_max, 
                                                                   df$indiv_MVPF, 
                                                                   cost_constraint),  # Initialize valid population
    seed = 42           # Seed for reproducibility
  )
  
  # Print the results of the optimal policy for this alpha
  cat("Maximized revenue for alpha =", alpha, ":", sum(ga_result@solution * R_scaled), "\n")
  
  # Check the MVPF of the final solution
  final_W <- ga_result@solution
  final_numerator <- sum(final_W * (R_scaled + B_scaled))
  final_denominator <- sum(final_W * (R_scaled - df$C))
  final_mvpf <- final_numerator / final_denominator
  
  # Calculate the total cost for the optimal policy
  total_cost <- sum(final_W * df$C)
  
  # Print the final MVPF and total cost
  cat("Final MVPF for alpha =", alpha, ":", final_mvpf, "\n")
  cat("Total cost of the optimal policy for alpha =", alpha, ":", total_cost, "\n")
  
  # Save the optimal allocation to the original dataframe
  column_name <- paste0("opt_2a_a", gsub("\\.", "", as.character(alpha)))
  final_W_df <- data.frame(t(final_W))
  names(final_W_df) <- column_name
  df[[column_name]] <- final_W_df[[column_name]]
  
  
  # Create a data frame for plotting convergence
  iterations <- seq_along(revenue_history)
  plot_data <- data.frame(
    Iteration = iterations,
    Total_Revenue = revenue_history,
    Aggregate_MVPF = mvpf_history
  )
  plot_data <- plot_data %>%
    mutate(Cumulative_Max_Revenue = cummax(Total_Revenue),
           Corresponding_MVPF = Aggregate_MVPF[match(Cumulative_Max_Revenue, Total_Revenue)])
  
  # Set the figure file name based on the alpha value
  figure_file <- paste0("GA_convergence_2b_a", gsub("\\.", "", as.character(alpha)), ".png")
  
  # Save the plot as a PNG file
  png(filename = file.path(figures_output_path, figure_file), width = 800, height = 600)
  
  par(mar = c(5, 4, 4, 5))  # Set margins for the plot
  
  # Generate the plot using twoord.plot
  twoord.plot(
    lx = plot_data$Iteration, ly = plot_data$Cumulative_Max_Revenue,
    rx = plot_data$Iteration, ry = plot_data$Corresponding_MVPF,
    lylim = range(plot_data$Cumulative_Max_Revenue, na.rm = TRUE),
    rylim = range(plot_data$Corresponding_MVPF, na.rm = TRUE),
    lcol = "blue", rcol = "red",
    xlab = "Iteration", ylab = "Revenue of Best Policy", rylab = "MVPF of Best Policy",
    main = paste("Genetic Algorithm Policy Convergence for alpha =", alpha),
    type = c("l", "l"),
    do.first = "grid()"
  )
  
  # Add axis labels
  axis(side = 2, at = pretty(plot_data$Cumulative_Max_Revenue), las = 0, col.axis = "blue", col = "blue")
  
  # Finalize and save the plot
  dev.off()
  
}






























