# Function to simulate y for a 2x3 contingency table
simulate_y <- function(probabilities, n) {
        # Check if probabilities sum to 1
        if (abs(sum(probabilities) - 1) == !0) {
                stop("Input probabilities must sum to 1")
        }
        
        # Define T matrix for the 2x3 contingency table
        T <- matrix(c(1, 1, 1, 0, 0, 0,
                      0, 0, 0, 1, 1, 1,
                      1, 0, 0, 1, 0, 0,
                      0, 1, 0, 0, 1, 0,
                      0, 0, 1, 0, 0, 1), 
                    nrow = 5, byrow = TRUE)
        
        # Simulate Z from the multinomial distribution
        Z <- rmultinom(1, size = n, prob = probabilities)
        
        # Compute Y = T * Z
        Y <- T %*% Z
        
        # Return the results
        list(Z = Z, Y = as.vector(Y))
}