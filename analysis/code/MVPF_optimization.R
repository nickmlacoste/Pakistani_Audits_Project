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



################################################
##### Revenue maximization problems ############
################################################

# Genetic Algorithm (max MVPF constraint only) --------------------------

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


# Genetic Algorithm (add maximum cost constraint) -------------------------

# Define the fitness function with additional cost constraint
evaluation_function <- function(W, R, B, C, MVPF_max, Cost_max) {
  mvpf <- mvpf_constraint(W, R, B, C, MVPF_max)
  total_cost <- sum(W * C)  # Calculate total cost for treated individuals
  
  # Check if MVPF and cost constraints are both satisfied
  if (mvpf <= MVPF_max && total_cost <= Cost_max) {
    total_revenue <- sum(W * R)  # Calculate total revenue if constraints are met
  } else {
    # Apply a penalty if either constraint is violated
    penalty <- 10000000 * (max(0, mvpf - MVPF_max) + max(0, total_cost - Cost_max))
    total_revenue <- sum(W * R) - penalty  # Apply penalty to revenue
  }
  
  # Store the total revenue and MVPF for this iteration
  revenue_history <<- c(revenue_history, sum(W * R))
  mvpf_history <<- c(mvpf_history, mvpf)
  
  return(total_revenue)
}

# Modify the population initialization function as needed
initialize_population_valid_fast <- function(object, R, B, C, MVPF_max, Cost_max, individual_MVPF) {
  # Create a population where only valid individuals are included
  n <- object@nBits
  pop <- matrix(0, nrow = object@popSize, ncol = n)
  
  # Step 1: Filter individuals with individual MVPF <= MVPF_max
  valid_indices <- which(individual_MVPF <= MVPF_max)
  valid_pool_size <- length(valid_indices)
  
  if (valid_pool_size == 0) {
    stop("No individuals with valid MVPF found.")
  }
  
  # Generate population, ensuring valid MVPF and cost constraint for each individual
  for (i in 1:object@popSize) {
    valid <- FALSE
    while (!valid) {
      # Randomly treat 10 individuals from the valid pool
      W <- rep(0, n)
      num_treated <- min(10, valid_pool_size)  # Treat at most 10 valid individuals
      treated_indices <- sample(valid_indices, num_treated)
      W[treated_indices] <- 1
      
      # Check if cumulative MVPF and cost are valid
      valid <- check_mvpf_constraint(W, R, B, C, MVPF_max) && sum(W * C) <= Cost_max
    }
    pop[i, ] <- W  # Store valid individual in population
  }
  
  return(pop)
}

# Maximum allowable cost (adjust this as needed)
MVPF_max <- 1.5
Cost_max <- 1500

# Run the Genetic Algorithm for binary W with custom initialization and the additional cost constraint
ga_result <- ga(
  type = "binary",
  fitness = function(W) evaluation_function(W, df$R, df$B, df$C, MVPF_max, Cost_max),
  nBits = nrow(df),   # Number of bits equal to number of individuals
  popSize = 200,      # Population size
  maxiter = 5000,     # Maximum number of generations
  run = 50,           # Stop if no improvement for 50 generations
  pmutation = 0.2,    # Higher mutation rate to allow for exploration
  population = function(object) initialize_population_valid_fast(object, 
                                                                 df$R, df$B, df$C, 
                                                                 MVPF_max, Cost_max, df$indiv_MVPF),  # Initialize valid population
  seed = 42           # Seed for reproducibility
)



# Print the results of the optimal policy
cat("Maximized revenue:", sum(ga_result@solution * df$R), "\n")

# Check the MVPF of the final solution
final_W <- ga_result@solution
final_numerator <- sum(final_W * (df$R + df$B))
final_denominator <- sum(final_W * (df$R - df$C))
final_mvp <- final_numerator / final_denominator

# Calculate the total cost for the optimal policy
total_cost <- sum(final_W * df$C)

# Print the final MVPF and total cost
cat("Final MVPF:", final_mvp, "\n")
cat("Total cost of the optimal policy:", total_cost, "\n")



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



# Define Genetic Algorithm ---------------------------------

# # Define the fitness function: total revenue
# fitness_function <- function(W, R) {
#   return(sum(W * R))  # Fitness is total revenue
# }

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
  
  # Step 1: Filter individuals with individual MVPF <= MVPF_max
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

# vectors to store revenues and MVPFs across iterations
revenue_history <- c()
mvpf_history <- c()

# Maximum allowable cost and MVPF
MVPF_max <- 1.5
Cost_max <- NULL

# Set to TRUE to enforce the cost constraint, FALSE to ignore it
cost_constraint <- FALSE  

ga_result <- ga(
  type = "binary",
  fitness = function(W) evaluation_function(W, df$R, df$B, df$C, MVPF_max, Cost_max, cost_constraint),
  nBits = nrow(df),   # Number of bits equal to number of individuals
  popSize = 200,      # Population size
  maxiter = 5000,     # Maximum number of generations
  run = 50,           # Stop if no improvement for 50 generations
  pmutation = 0.2,    # Higher mutation rate to allow for exploration
  population = function(object) initialize_population_valid_fast(object, 
                                                                 df$R, df$B, df$C, 
                                                                 MVPF_max, Cost_max, 
                                                                 df$indiv_MVPF, 
                                                                 cost_constraint),  # Initialize valid population
  seed = 42           # Seed for reproducibility
)

# Print the results of the optimal policy
cat("Maximized revenue:", sum(ga_result@solution * df$R), "\n")

# Check the MVPF of the final solution
final_W <- ga_result@solution
final_numerator <- sum(final_W * (df$R + df$B))
final_denominator <- sum(final_W * (df$R - df$C))
final_mvpf <- final_numerator / final_denominator

# Calculate the total cost for the optimal policy
total_cost <- sum(final_W * df$C)

# Print the final MVPF and total cost
cat("Final MVPF:", final_mvpf, "\n")
cat("Total cost of the optimal policy:", total_cost, "\n")


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

par(mar = c(5, 4, 4, 5))

png(filename = file.path(figures_output_path, "GA_convergence_2a.png"), 
    width = 800, height = 600)

twoord.plot(
  lx = plot_data$Iteration, ly = plot_data$Cumulative_Max_Revenue,
  rx = plot_data$Iteration, ry = plot_data$Corresponding_MVPF,
  lylim = range(plot_data$Cumulative_Max_Revenue, na.rm = TRUE),
  rylim = range(plot_data$Corresponding_MVPF, na.rm = TRUE),
  lcol = "blue", rcol = "red",
  xlab = "Iteration", ylab = "Revenue of Best Policy", rylab = "MVPF of Best Policy",
  main = "Genetic Algorithm Policy Convergence",
  type = c("l", "l"),
  do.first = "grid()"
)
axis(side = 2, at = pretty(plot_data$Cumulative_Max_Revenue), 
     las = 0, col.axis = "blue", col = "blue")

dev.off()

# Policy 2b: Revenue max, MVPF and cost constraints -----------------------

# vectors to store revenues and MVPFs across iterations
revenue_history <- c()
mvpf_history <- c()

# Maximum allowable cost and MVPF
MVPF_max <- 1.5
Cost_max <- 200

# Set to TRUE to enforce the cost constraint, FALSE to ignore it
cost_constraint <- TRUE  

ga_result <- ga(
  type = "binary",
  fitness = function(W) evaluation_function(W, df$R, df$B, df$C, MVPF_max, Cost_max, cost_constraint),
  nBits = nrow(df),   # Number of bits equal to number of individuals
  popSize = 200,      # Population size
  maxiter = 5000,     # Maximum number of generations
  run = 50,           # Stop if no improvement for 50 generations
  pmutation = 0.2,    # Higher mutation rate to allow for exploration
  population = function(object) initialize_population_valid_fast(object, 
                                                                 df$R, df$B, df$C, 
                                                                 MVPF_max, Cost_max, 
                                                                 df$indiv_MVPF, 
                                                                 cost_constraint),  # Initialize valid population
  seed = 42           # Seed for reproducibility
)

# Print the results of the optimal policy
cat("Maximized revenue:", sum(ga_result@solution * df$R), "\n")

# Check the MVPF of the final solution
final_W <- ga_result@solution
final_numerator <- sum(final_W * (df$R + df$B))
final_denominator <- sum(final_W * (df$R - df$C))
final_mvpf <- final_numerator / final_denominator

# Calculate the total cost for the optimal policy
total_cost <- sum(final_W * df$C)

# Print the final MVPF and total cost
cat("Final MVPF:", final_mvpf, "\n")
cat("Total cost of the optimal policy:", total_cost, "\n")


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

par(mar = c(5, 4, 4, 5))

png(filename = file.path(figures_output_path, "GA_convergence_2b.png"), 
    width = 800, height = 600)

twoord.plot(
  lx = plot_data$Iteration, ly = plot_data$Cumulative_Max_Revenue,
  rx = plot_data$Iteration, ry = plot_data$Corresponding_MVPF,
  lylim = range(plot_data$Cumulative_Max_Revenue, na.rm = TRUE),
  rylim = range(plot_data$Corresponding_MVPF, na.rm = TRUE),
  lcol = "blue", rcol = "red",
  xlab = "Iteration", ylab = "Revenue of Best Policy", rylab = "MVPF of Best Policy",
  main = "Genetic Algorithm Policy Convergence",
  type = c("l", "l"),
  do.first = "grid()"
)
axis(side = 2, at = pretty(plot_data$Cumulative_Max_Revenue), 
     las = 0, col.axis = "blue", col = "blue")

dev.off()






























