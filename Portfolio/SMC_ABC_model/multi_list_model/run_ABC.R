# Load required libraries
library(sirt)
library(LaplacesDemon)
library(MASS)

# Source required functions
source("required_functions.R")  
source("Simulate.R")  
source("smc_abc_generic.R")  

# Set seed for reproducibility
set.seed(19)  

# Define observed data (from Auckland Diabetes Study)
observed_Y <- c(1183, 12265, 3276, 654, 51, 366, 91, 40, 4, 56, 14)  # Observed counts for 11 capture histories

cat("Observed Capture Histories (Y):\n")
print(observed_Y)

# Extra arguments (fixed N = 43000)
extra_args <- list(
        observed_Y = observed_Y,  # Observed list counts
        return_summ = TRUE
)

# Run the SMC ABC algorithm
set.seed(42)
results <- smc_abc_generic(
        N = 1000,                 
        a = 0.5,                  
        acc_rate_stop = 0.01,     
        prior_sim = prior_sim_trans,  # Normal prior for beta
        prior_eval = prior_eval,      # Evaluate beta under Normal(0, 10^2)
        distance_fun = distance_capture,  # Distance function comparing capture histories
        extra_args = extra_args
)

# Transform N_logit back to N for interpretation
N_logit <- results$theta[, 1]
N <- 20000 + (80000 - 20000) * exp(N_logit) / (1 + exp(N_logit))
print(N)

# Print results
cat("\nEstimated Posterior for Beta:\n")
print(results$theta[,-1])  # Posterior samples of beta
colMeans(results$theta[, -1])

beta_post = results$theta[, -1]


# Load ggplot2
library(ggplot2)

# Convert to a data frame
beta_1_df <- data.frame(Value = beta_post[,1])

# Plot histogram with mean line
ggplot(beta_1_df, aes(x = Value)) +
        geom_histogram(color = 'black', fill = 'lightblue', bins = 30) +  # Histogram
        geom_vline(aes(xintercept = mean(Value)), color = "blue", linetype = "dashed", size = 1) +  # Mean line
        labs(title = "Posterior Distribution of Beta_1", x = "Beta_1 Values", y = "Count") +
        theme_minimal()


# Convert to a data frame
beta_3_df <- data.frame(Value = beta_post[,3])

# Plot histogram with mean line
ggplot(beta_3_df, aes(x = Value)) +
        geom_histogram(color = 'black', fill = 'lightblue', bins = 30) +  # Histogram
        geom_vline(aes(xintercept = mean(Value)), color = "blue", linetype = "dashed", size = 1) +  # Mean line
        labs(title = "Posterior Distribution of Beta_3", x = "Beta_3 Values", y = "Count") +
        theme_minimal()


# Convert to a data frame
beta_5_df <- data.frame(Value = beta_post[,4])

# Plot histogram with mean line
ggplot(beta_5_df, aes(x = Value)) +
        geom_histogram(color = 'black', fill = 'lightblue', bins = 30) +  # Histogram
        geom_vline(aes(xintercept = median(Value)), color = "blue", linetype = "dashed", size = 1) +  # Mean line
        labs(title = "Posterior Distribution of Beta_5", x = "Beta_5 Values", y = "Count") +
        theme_minimal()




