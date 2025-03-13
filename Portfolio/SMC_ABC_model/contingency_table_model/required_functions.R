# Function to simulate parameters (theta)
prior_sim_trans <- function(extra_args) {
        theta <- rdirichlet(1, rep(1, 6)) 
        return(theta)
}


# Function to evaluate prior density
prior_eval <- function(theta, extra_args) {
        f <- ddirichlet(theta, rep(1, 6))
        return(f)
}


# Distance function to compare simulated and observed marginals
distance_twoway <- function(theta, extra_args) {
        # Simulate data from theta using the simulate_y function
        sim_result <- simulate_y(theta, extra_args$n)
        sim_Y <- sim_result$Y
        
        # Compute Euclidean distance between observed and simulated Y (marginals)
        observed_Y <- extra_args$observed_Y
        dist <- sum((sim_Y - observed_Y)^2)
        
        # Return the distance as part of a list
        return(list(dist = dist))
}

