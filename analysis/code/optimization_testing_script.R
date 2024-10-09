# 
"
Simulations for testing different optimization algorithms
"
#




library(tidyverse)
library(GA)
library(plotrix)



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
calculate_total_mvpf <- function(df_subset) {
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
result_mini_batch <- mini_batch_sgd(df, bar_R = 3000, batch_size = 50, max_iter = 2500)

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
                                    bar_R = 3000, 
                                    bar_C = 500, 
                                    batch_size = 64, 
                                    max_iter = 2500)

# Plot the objective function over iterations
plot(result_mbsgd$obj_values, type = "l", col = "blue", 
     main = "Objective Function Over Iterations (Mini-batch with Constraints)", 
     xlab = "Iteration", ylab = "MVPF Value")


# Problem 2a: revenue maximization, MVPF constraint (MBSGD) --------------------------

# Initialize treatment with MVPF constraint
initialize_treatment_with_mvp_constraint <- function(df, bar_MVPF) {
  # Sort individuals by R_i in descending order
  sorted_indices <- order(df$R, decreasing = TRUE)
  
  # Initialize treatment to zeros (no one treated)
  treatment <- rep(0, nrow(df))
  
  total_R <- 0
  total_C <- 0
  total_B <- 0
  
  for (i in sorted_indices) {
    # Calculate potential MVPF if we add this individual
    new_total_R <- total_R + df$R[i]
    new_total_C <- total_C + df$C[i]
    new_total_B <- total_B + df$B[i]
    
    if (new_total_R > new_total_C) {  # Ensure denominator is positive
      mvp <- (new_total_R + new_total_B) / (new_total_R - new_total_C)
      if (mvp <= bar_MVPF) {
        treatment[i] <- 1  # Assign treatment
        total_R <- new_total_R
        total_C <- new_total_C
        total_B <- new_total_B
      }
    }
  }
  
  return(treatment)
}

# Mini-batch SGD with MVPF constraint
mini_batch_sgd_mvp <- function(df, bar_MVPF, batch_size, learning_rate = 0.01, max_iter) {
  
  # Initialize treatment with MVPF constraint
  treatment <- initialize_treatment_with_mvp_constraint(df, bar_MVPF)
  
  # Store objective function values (total revenue)
  obj_values <- numeric(max_iter)
  
  for (iter in 1:max_iter) {
    # Randomly select a mini-batch
    batch_indices <- sample(1:nrow(df), batch_size)
    
    # For each individual in the mini-batch, calculate the gradient and update treatment
    for (i in batch_indices) {
      grad <- gradient(df$R, df$B, df$C, treatment, i)
      
      # Update treatment based on maximizing revenue
      if (grad > 0 && treatment[i] == 0) {
        treatment[i] <- 1  # Turn on treatment if gradient is positive and individual not treated
      } else if (grad < 0 && treatment[i] == 1) {
        treatment[i] <- 0  # Turn off treatment if gradient is negative and individual is treated
      }
    }
    
    # Calculate total revenue and MVPF after updates
    total_R <- sum(treatment * df$R)
    total_C <- sum(treatment * df$C)
    total_B <- sum(treatment * df$B)
    
    # Ensure denominator is positive for MVPF calculation
    if (total_R > total_C) {
      mvp <- (total_R + total_B) / (total_R - total_C)
      
      # Ensure the MVPF constraint is satisfied
      if (mvp > bar_MVPF) {
        # Adjust treatment to reduce MVPF by turning off high-cost individuals
        top_c_index <- order(df$C, decreasing = TRUE)
        for (j in top_c_index) {
          if (treatment[j] == 1) {
            treatment[j] <- 0
            total_R <- sum(treatment * df$R)
            total_C <- sum(treatment * df$C)
            total_B <- sum(treatment * df$B)
            if (total_R > total_C) {
              mvp <- (total_R + total_B) / (total_R - total_C)
              if (mvp <= bar_MVPF) break
            }
          }
        }
      }
    }
    
    # Store the current total revenue
    obj_values[iter] <- total_R
  }
  
  # Calculate final metrics
  total_revenue <- sum(treatment * df$R)
  total_cost <- sum(treatment * df$C)
  num_treated <- sum(treatment)
  final_mvp <- ifelse(total_revenue > total_cost, (total_revenue + sum(treatment * df$B)) / (total_revenue - total_cost), NA)
  
  # Print out the final results
  cat("Final treatment allocation (mini-batch with MVPF constraint):", num_treated, "\n")
  cat("Total Revenue (sum R):", total_revenue, "\n")
  cat("Total Cost (sum C):", total_cost, "\n")
  cat("Final MVPF:", final_mvp, "\n")
  
  # Return the final treatment allocation, objective function values, and metrics
  return(list(treatment = treatment, obj_values = obj_values, 
              total_revenue = total_revenue, total_cost = total_cost, 
              num_treated = num_treated, final_mvp = final_mvp))
}

# Run the mini-batch SGD algorithm to maximize revenue with MVPF constraint
result_mbsgd_mvp <- mini_batch_sgd_mvp(df,
                                       bar_MVPF = 1.5,  # Example threshold for MVPF
                                       batch_size = 64, 
                                       max_iter = 2500)

# Plot the objective function over iterations
plot(result_mbsgd_mvp$obj_values, type = "l", col = "green", 
     main = "Total Revenue Over Iterations (Mini-batch with MVPF Constraint)", 
     xlab = "Iteration", ylab = "Total Revenue")


# Nonlinear knapsack (max revenue s.t. MVPF max) ------------------------------

library(nloptr)

# Define objective function: maximize revenue
objective_function <- function(W, R) {
  return(-sum(W * R))  # Negative because nloptr minimizes by default
}

# Define constraint: MVPF <= bar_MVPF
mvpf_constraint <- function(W, R, B, C, bar_MVPF) {
  numerator <- sum(W * (R + B))
  denominator <- sum(W * (R - C))
  return(numerator / denominator - bar_MVPF)
}

R <- df$R
B <- df$B
C <- df$C
MVPF_max <- 1.5

W0 <- rep(0.5, length(R))  # Continuous initial guess

# Solve the nonlinear problem
result <- nloptr(
  x0 = W0,
  eval_f = function(W) objective_function(W, R),
  lb = rep(0, length(R)), ub = rep(1, length(R)),  # bounds on W
  eval_g_ineq = function(W) mvpf_constraint(W, R, B, C, MVPF_max),
  opts = list("algorithm" = "NLOPT_LD_MMA", "maxeval" = 1000)
)

# Print result
cat("Optimized treatment allocation (W):", result$solution, "\n")
cat("Optimized revenue:", -result$objective, "\n")

# Genetic Algorithm ------------

# Define the fitness function: total revenue
fitness_function <- function(W, R) {
  return(sum(W * R))  # Fitness is total revenue
}

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

# vectors to store revenues and MVPFs across iterations
revenue_history <- c()
mvpf_history <- c()

# Combine the fitness and constraint in the evaluation function
evaluation_function <- function(W, R, B, C, MVPF_max) {
  mvpf <- mvpf_constraint(W, R, B, C, MVPF_max)
  
  # Apply a soft penalty based on how much the constraint is violated
  if (mvpf <= MVPF_max) {
    total_revenue <- sum(W * R)  # Calculate total revenue
  } else {
    total_revenue <- sum(W * R) - 10000000 * (mvpf - MVPF_max)  # Apply penalty
  }
  
  # Store the total revenue and MVPF for this iteration
  revenue_history <<- c(revenue_history, sum(W * R))
  mvpf_history <<- c(mvpf_history, mvpf)
  
  return(total_revenue)
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

initialize_population_valid_fast <- function(object, R, B, C, MVPF_max, individual_MVPF) {
  # Create a population where only valid individuals are included
  n <- object@nBits
  pop <- matrix(0, nrow = object@popSize, ncol = n)
  
  # Step 1: Filter individuals with individual MVPF <= MVPF_max
  valid_indices <- which(individual_MVPF <= MVPF_max)
  valid_pool_size <- length(valid_indices)
  
  if (valid_pool_size == 0) {
    stop("No individuals with valid MVPF found.")
  }
  
  # Generate population, ensuring valid MVPF constraint for each individual
  for (i in 1:object@popSize) {
    valid <- FALSE
    while (!valid) {
      # Randomly treat 10 individuals from the valid pool
      W <- rep(0, n)
      num_treated <- min(10, valid_pool_size)  # Treat at most 10 valid individuals
      treated_indices <- sample(valid_indices, num_treated)
      W[treated_indices] <- 1
      
      # Check if cumulative MVPF is valid
      valid <- check_mvpf_constraint(W, R, B, C, MVPF_max)
    }
    pop[i, ] <- W  # Store valid individual in population
  }
  
  return(pop)
}

MVPF_max <- 1.5

# Run the Genetic Algorithm for binary W with custom initialization
ga_result <- ga(
  type = "binary",
  fitness = function(W) evaluation_function(W, df$R, df$B, df$C, MVPF_max),
  nBits = nrow(df),   # Number of bits equal to number of individuals
  popSize = 200,      # Population size
  maxiter = 5000,     # Maximum number of generations
  run = 50,          # Stop if no improvement for 50 generations
  pmutation = 0.2,    # Higher mutation rate to allow for exploration
  population = function(object) initialize_population_valid_fast(object, 
                                                                 df$R, df$B, df$C, 
                                                                 MVPF_max, df$indiv_MVPF),  # Initialize valid population
  seed = 42           # Seed for reproducibility
)

# Print the results
#cat("Optimal treatment allocation (W):", ga_result@solution, "\n")
cat("Maximized revenue:", sum(ga_result@solution * df$R), "\n")

# Check the MVPF of the final solution
final_W <- ga_result@solution
final_numerator <- sum(final_W * (df$R + df$B))
final_denominator <- sum(final_W * (df$R - df$C))
final_mvp <- final_numerator / final_denominator
cat("Final MVPF:", final_mvp, "\n")

# Create a data frame for plotting
iterations <- seq_along(revenue_history)  # Sequence of iterations
plot_data <- data.frame(
  Iteration = iterations,
  Total_Revenue = revenue_history,
  Aggregate_MVPF = mvpf_history
)
plot_data <- plot_data %>%
  mutate(Cumulative_Max_Revenue = cummax(Total_Revenue),
         Corresponding_MVPF = Aggregate_MVPF[match(Cumulative_Max_Revenue, Total_Revenue)])

par(mar = c(5, 4, 4, 5))

twoord.plot(
  lx = plot_data$Iteration, ly = plot_data$Cumulative_Max_Revenue,
  rx = plot_data$Iteration, ry = plot_data$Corresponding_MVPF,
  lylim = range(plot_data$Cumulative_Max_Revenue, na.rm = TRUE),
  rylim = range(plot_data$Corresponding_MVPF, na.rm = TRUE),
  lcol = "blue", rcol = "red",
  xlab = "Iteration", ylab = "Cumulative Max Revenue", rylab = "Aggregate MVPF",
  main = "Genetic Algorithm Policy Convergence",
  type = c("l", "l"),
  do.first = "grid()"
)
axis(side = 2, at = pretty(plot_data$Cumulative_Max_Revenue), 
     las = 0, col.axis = "blue", col = "blue")



















