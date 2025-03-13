
library(sirt)
library(LaplacesDemon)

source("required_functions.R")
source("Simulate.R")
source("smc_abc_generic.R")
source("smc_abc_dirichlet.R")


# Simulate the observed data
set.seed(19)
probabilities <- c(0.1, 0.2, 0.2, 0.2, 0.2, 0.1)
n <- 100  

# Simulate observed marginals (Y)
simulated_result <- simulate_y(probabilities, n)
observed_Y <- simulated_result$Y  # Marginal totals

cat("Observed Marginals (Y):\n")
print(observed_Y)

# Extra arguments
extra_args <- list(
        n = n,
        observed_Y = observed_Y, 
        return_summ = TRUE
)

# Run the SMC ABC algorithm
set.seed(42)
results <- smc_abc_dirichlet(
        N = 1000,                 
        a = 0.5,                  
        acc_rate_stop = 0.01,     
        prior_sim = prior_sim_trans,
        prior_eval = prior_eval,
        distance_fun = distance_twoway,
        extra_args = extra_args
)


theta_post = results$theta
dist_post = results$dist


theta_prior = rdirichlet(1000, rep(1,6))

hist(theta_post[,1])
x11()
hist(theta_prior[,1])


library(ggplot2)
library(tidyr)
library(dplyr)

# Convert theta_post matrix to a long-format data frame
theta_df <- as.data.frame(theta_post) %>%
        mutate(id = row_number()) %>%  # Add an ID column for gathering
        pivot_longer(cols = starts_with("V"), names_to = "Parameter", values_to = "Value")

# Plot histogram with faceting
ggplot(theta_df, aes(x = Value)) +
        geom_histogram(color = 'black', fill = 'lightblue', bins = 20) +
        geom_vline(aes(xintercept = mean(Value)), color = "blue", linetype = "dashed", size = 1) +
        facet_wrap(~ Parameter, scales = "free") +  # Facet by Parameter
        labs(title = "Histograms of Six Parameters", x = "Values", y = "Count") +
        theme_minimal()
        
        


