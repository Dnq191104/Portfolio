simulate_capture_histories <- function(theta, extra_args) {
        N_logit <- theta[1]
        beta <- theta[-1]
        
        # Transform N_logit to N and round
        a <- 20000
        b <- 80000
        N <- round(a + (b - a) * exp(N_logit) / (1 + exp(N_logit)))  # Round to nearest integer
        
        
        #N <- round(theta[1])  # Round N to nearest integer
        #beta <- theta[-1]
        
        # Code to generate T matrix and X matrix
        ## -----------------------------------------------------------------------------
        
        sorder=NULL
        forder=list(c(1,2,3), c(4,5,6))
        ## Construct all latent histories whose counts form a vector of length 16=2^4, which, however, 
        ## is not completely observable.
        nlist <- 4
        expandstr <- paste("expand.grid(", paste(rep("0:1", nlist), collapse=", "), ")")
        true.hists <- as.matrix(eval(parse(text=expandstr))[, nlist:1])
        colnames(true.hists) <- NULL
        
        ## Find Tmat that combines the observed vector of counts and the underlying vector
        ## of latent counts, and find observable histories. These are the same for all different models considered.
        ## Observable histories (3 vague hists and 8 fully-observed hists):
        obs.hists <- c("01..", "0.1.", "0..1",
                       "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
        ## Tmat, as shown in Sutherland and Schwarz (2005)
        ## This can be sorted out via the relationship between the observed and latent histories.
        Tmat <- matrix(0, nrow=11, ncol=16)
        Tmat[1, 1:8] <- c(0, 0, 0, 0, 1, 1, 1, 1)
        Tmat[2, 1:8] <- c(0, 0, 1, 1, 0, 0, 1, 1)
        Tmat[3, 1:8] <- c(0, 1, 0, 1, 0, 1, 0, 1)
        for(i in 4:11) Tmat[i, i+5] <- 1
        
        ## -----------------------------------------------------------------------------
        ## Construct matrix X that relates muvec, the mean vector of the Poisson distributions to
        ## betavec, the vector of model parameters: muvec = exp(X %*% betavec).
        ## First, consider main effects:
        mmat <- true.hists
        
        ## Now treat six first-order interaction effects (GP, GO, GD, PO, PD, OD) separately.
        fpair <- combn(1:4, 2)
        fmat <- matrix(0, nrow=16, ncol=6)
        for(i in 1:16){
                for(j in 1:6) if(prod(true.hists[i,c(fpair[,j])])==1) fmat[i,j] <- 1
        }
        
        ## Start constructing matrix X:
        X <- mmat
        ## Now let some of the first-order interactions be identical following the given model. 
        nf <- length(forder)
        for(i in 1:nf){
                if(length(forder[[i]])==1) X <- cbind(X, fmat[,forder[[i]]])
                else X <- cbind(X, rowSums(fmat[,forder[[i]]]))
        } 
        
        ## Add second-order interactions when necessary
        if(!is.null(sorder)){
                spair <- matrix(c(1,2,3, 1,2,4, 1,3,4, 2,3,4), nrow=4, byrow=TRUE)
                smat <- matrix(0, nrow=16, ncol=4)
                for(i in 1:16){
                        for(j in 1:4) if(prod(true.hists[i,c(spair[j,])])==1) smat[i,j] <- 1
                }
                ## Now let some of the second-order interactions be identical following the given model. 
                ns <- length(sorder)
                for(i in 1:ns){
                        if(length(sorder[[i]])==1) X <- cbind(X, smat[,sorder[[i]]])
                        else X <- cbind(X, rowSums(smat[,sorder[[i]]]))
                } }
        ## -----------------------------------------------------------------------------
        
        
        
        # Convert beta parameters to probabilities using a softmax transformation
        exp_beta <- exp(X%*%beta)
        probabilities <- exp_beta / sum(exp_beta)  # Normalize probabilities
        
        # Simulate latent capture histories (Ensure it's a column vector)
        Z <- rmultinom(1, size = N, prob = probabilities) 
        
        # Compute observed counts (Y)
        Y <- Tmat %*% Z
        
        return(list(Z = Z, Y = as.vector(Y)))  # Return both latent and observed counts
}
