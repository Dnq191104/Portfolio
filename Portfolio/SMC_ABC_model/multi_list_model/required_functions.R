# Function to simulate parameters (beta)
#prior_sim_trans <- function(extra_args) {
#        # Sample N_logit from a diffuse normal prior (unbounded)
#        N_logit <- rnorm(1, mean = 0, sd = 10)  # Diffuse prior for logit-transformed N
#        # Transform back to N in [20000, 80000]
#        a <- 20000
#        b <- 80000
#        N <- a + (b - a) * exp(N_logit) / (1 + exp(N_logit))
#        # Sample each beta from N(0, 10^2)
#        beta <- rnorm(6, mean = 0, sd = 5)
#        return(c(N_logit,beta))
#}

prior_sim_trans <- function(extra_args) {
        # Sample N from Uniform(20000, 80000)
        N <- runif(1, 20000, 80000)
        ### logit tranformation for N
        N_logit <- log((N-20000)/(80000-N))
        # Sample 6 beta parameters from N(0, 10^2)
        beta <- rnorm(6, mean = 0, sd = 10)
        
        ### Return N_logit with bet
        return(c(N_logit, beta))  # Return N + betas
}


# Function to evaluate prior density
#prior_eval <- function(theta, extra_args) {
#        N_logit <- theta[1]
#        beta <- theta[-1]
#        
#        # Transform N_logit to N for bounds checking
#        a <- 20000
#        b <- 80000
#        N <- a + (b - a) * exp(N_logit) / (1 + exp(N_logit))
#        
#        # Uniform prior on N: dunif on [20000, 80000], adjust for transformation
#        if (N < 20000 || N > 80000) return(0)  # Outside bounds
#        log_jacobian <- log(b - a) + N_logit - 2 * log(1 + exp(N_logit))  # Jacobian of logit transform
#        log_prior_N <- dunif(N, 20000, 80000, log = TRUE) + log_jacobian
#        
#        # Normal priors on beta
#        log_prior_beta <- sum(dnorm(beta, mean = 0, sd = 5, log = TRUE))
#        
#        return(exp(log_prior_N + log_prior_beta))  # Total prior density
#}

prior_eval <- function(theta, extra_args) {
        N_logit <- theta[1]
        beta <- theta[-1]
        
        log_jacobian <- N_logit - 2 * log(1 + exp(N_logit))  # Jacobian of logit transform
        # Normal priors on beta
        log_prior_beta <- sum(dnorm(beta, mean = 0, sd = 5, log = TRUE))
        
        return(exp(log_jacobian + log_prior_beta))  # Total prior density
}


# Distance function
distance_capture <- function(theta, extra_args) {
        simulated_data <- simulate_capture_histories(theta, extra_args)
        sim_Y <- simulated_data$Y
        obs_Y <- extra_args$observed_Y
        dist <- sqrt(sum((sim_Y - obs_Y)^2))  # Euclidean distance
        return(list(dist = dist, sim_summ = sim_Y))
}

