# 
"
Simulations for testing different optimization algorithms
"
#




library(dplyr)



# generate simulated data ---------------------
generate_dataset <- function(n) {
  set.seed(42)  # For reproducibility
  R <- runif(n, min = 5, max = 20)  # Revenue (R_i)
  C <- runif(n, min = 1, max = 10)  # Cost (C_i)
  B <- runif(n, min = 1, max = 10)  # Burden (B_i)
  
  # Calculate individual MVPF
  indiv_MVPF <- (R + B) / (R - C)
  
  # Return the dataset as a data frame
  return(data.frame(R, B, C, indiv_MVPF))
}

df <- generate_dataset(n=1000)

# Function to calculate total MVPF for a given selection of individuals
calculate_total_mvp <- function(df_subset) {
  total_benefit <- sum(df_subset$R + df_subset$B)
  total_net_cost <- sum(df_subset$R) - sum(df_subset$C)
  return(total_benefit / total_net_cost)
}

# Greedy algorithm -----------------------

# Function to perform the greedy algorithm, ignoring individuals with negative MVPF
greedy_algorithm <- function(df, revenue_constraint) {
  # Filter out individuals with negative individual MVPF
  df <- df[df$indiv_MVPF >= 0, ]
  
  # Sort by individual MVPF
  df_sorted <- df[order(df$indiv_MVPF), ]
  
  # Select individuals in ascending order of MVPF until the revenue constraint is met
  cumulative_revenue <- 0
  greedy_subset <- data.frame()
  
  for (i in 1:nrow(df_sorted)) {
    if (cumulative_revenue >= revenue_constraint) break
    greedy_subset <- rbind(greedy_subset, df_sorted[i, ])
    cumulative_revenue <- cumulative_revenue + df_sorted$R[i]
  }
  
  # Calculate the MVPF for the greedy approach
  greedy_mvp <- calculate_total_mvp(greedy_subset)
  
  return(list(greedy_mvp = greedy_mvp, greedy_subset = greedy_subset))
}

greedy_solution <- greedy_algorithm(df, revenue_constraint = 3000)
cat("Greedy Algorithm MVPF:", greedy_solution$greedy_mvp, "\n")


# Mini-batch stochastic gradient descent with individual updating ------------------

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

# Function to determine initial treatment allocation greedily (this increases speed by not relying on
# random sampling to determine the initial allocation, which in large samples may take a long time to
# produce a valid initial allocation)
initialize_treatment <- function(df, bar_R) {
  # Sort individuals by R_i in descending order
  sorted_indices <- order(df$R, decreasing = TRUE)
  
  # Initialize treatment to zeros (no one treated)
  treatment <- rep(0, nrow(df))
  
  # Greedily assign treatment starting from the highest R_i
  total_R <- 0
  for (i in sorted_indices) {
    treatment[i] <- 1  # Assign treatment
    total_R <- total_R + df$R[i]
    if (total_R >= bar_R) break  # Stop when the constraint is satisfied
  }
  
  # Randomly assign treatment to the remaining individuals
  remaining_indices <- setdiff(1:nrow(df), which(treatment == 1))
  treatment[remaining_indices] <- sample(0:1, length(remaining_indices), replace = TRUE)
  
  return(treatment)
}

# Mini-batch SGD implementation with individual updates based on gradients
mini_batch_sgd <- function(df, bar_R, batch_size, learning_rate = 0.01, max_iter) {
  
  # Initial random treatment allocation (0 or 1)
  n <- nrow(df)
  treatment <- initialize_treatment(df, bar_R)

  # Store objective function values
  obj_values <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    # Randomly select a mini-batch
    batch_indices <- sample(1:n, batch_size)
    
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
    
    # Ensure that the constraint is satisfied (adjust if violated)
    if (sum(treatment * df$R) < bar_R) {
      # Restore treatment for individuals with highest R to meet the constraint
      top_r_index <- order(df$R, decreasing = TRUE)
      for (j in top_r_index) {
        if (treatment[j] == 0) {
          treatment[j] <- 1
          if (sum(treatment * df$R) >= bar_R) break
        }
      }
    }
    
    # Calculate and store the objective function value
    obj_values[iter] <- objective_function(df$R, df$B, df$C, treatment)
  }
  
  # Return the final treatment allocation and objective function values
  return(list(treatment = treatment, obj_values = obj_values))
}

# Run the mini-batch SGD algorithm on the generated dataset with \bar{R} = 500
result_mini_batch <- mini_batch_sgd(df, bar_R = 3000, batch_size = 50, max_iter = 15000)

# Print results
cat("Final treatment allocation (mini-batch):", sum(result_mini_batch$treatment), "\n")
cat("Final objective function value (mini-batch):", tail(result_mini_batch$obj_values, 1), "\n")

# Plot the objective function over iterations
plot(result_mini_batch$obj_values, type = "l", col = "red", main = "Objective Function Over Iterations (Mini-batch)", 
     xlab = "Iteration", ylab = "MVPF Value")




# Adding the optional cost constraint to the mini-batch SGD algorithm ---------------------------

initialize_treatment_with_constraints <- function(df, bar_R, bar_C) {
  # Sort individuals by R_i in descending order
  sorted_indices <- order(df$R, decreasing = TRUE)
  
  # Initialize treatment to zeros (no one treated)
  treatment <- rep(0, nrow(df))
  
  # Greedily assign treatment starting from the highest R_i
  total_R <- 0
  total_C <- 0
  for (i in sorted_indices) {
    if (total_R < bar_R && (total_C + df$C[i]) <= bar_C) {
      treatment[i] <- 1  # Assign treatment
      total_R <- total_R + df$R[i]
      total_C <- total_C + df$C[i]
    }
    if (total_R >= bar_R && total_C <= bar_C) break  # Stop when both constraints are satisfied
  }
  
  # Randomly assign treatment to the remaining individuals while respecting \bar{C}
  remaining_indices <- setdiff(1:nrow(df), which(treatment == 1))
  for (i in remaining_indices) {
    if ((total_C + df$C[i]) <= bar_C) {
      treatment[i] <- sample(0:1, 1)  # Randomly assign 0 or 1
      if (treatment[i] == 1) total_C <- total_C + df$C[i]
    }
  }
  
  return(treatment)
}


# Mini-batch SGD with additional constraint on C
mini_batch_sgd_cost <- function(df, bar_R, bar_C, batch_size, learning_rate = 0.01, max_iter) {
  
  # Initialize treatment with both constraints
  treatment <- initialize_treatment_with_constraints(df, bar_R, bar_C)
  
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
    
    # Ensure that the constraint \sum_i C_i <= \bar{C} is satisfied
    if (sum(treatment * df$C) > bar_C) {
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
  cat("Final treatment allocation (mini-batch with constraints):", num_treated, "\n")
  cat("Total Revenue (sum R):", total_revenue, "\n")
  cat("Total Cost (sum C):", total_cost, "\n")
  cat("Final Objective Function Value:", final_obj_value, "\n")
  
  # Return the final treatment allocation, objective function values, and metrics
  return(list(treatment = treatment, obj_values = obj_values, 
              total_revenue = total_revenue, total_cost = total_cost, 
              num_treated = num_treated, final_obj_value = final_obj_value))
}

# Run the modified mini-batch SGD algorithm with both constraints
result_mbsgd <- mini_batch_sgd_cost(df,
                                    bar_R = 5000, 
                                    bar_C = 1000, 
                                    batch_size = 64, 
                                    max_iter = 20000)

# Plot the objective function over iterations
plot(result_mbsgd$obj_values, type = "l", col = "blue", 
     main = "Objective Function Over Iterations (Mini-batch with Constraints)", 
     xlab = "Iteration", ylab = "MVPF Value")


# Integer Linear Programming --------------------------

library(lpSolve)

























