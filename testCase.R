## Testcase copied from coursers forum, created by James D. Hasselman 

source("cachematrix.R")

cachematrixtest <- function() {
    # Create a matrix to test. I choose random values
    r <- 1000
    min <- -10000
    max <- 10000
    mat <- matrix(runif(r^2, min = min, max = max), r)
    l <- makeCacheMatrix(mat)
    
    # TRUE the first time
    if(!is.null(l$getInverse())) { 
        stop("The inverse of the cache matrix should be NULL when it is first created.")
    } 
    
    
    s <- sum(colSums(cacheSolve(l) %*% l$get()))
    # This should be TRUE.
    if(!all.equal(s, r)) {
        stop("cacheSolve is not correctly calculating the inverse.")
    }
    
    # If you run it twice you should get the same result and it should read
    # "getting cached data" in the console
    
    # FALSE the second time
    if(is.null(l$getInverse())) {
        stop("the inverse is not being cached.")
    }
    
    # Here we can check that R is saving computation time
    l <- makeCacheMatrix(mat)
    uncached <- system.time(cacheSolve(l)) # Around 4.6 seconds the first time
    cached <- system.time(cacheSolve(l)) # 0 seconds the second time
    
    print(paste("uncached time:", uncached["elapsed"]))
    print(paste("cached time:", cached["elapsed"]))
    print("Note: These times should be significantly different.")
    
    if(uncached["elapsed"] <= cached["elapsed"]) {
        stop("the caching doesn't appear to work. please test it manually
         to be certain.")
    }
    
    old_l = l$get()
    l$set(matrix(c(1), nrow=2, ncol=2))
    # Make sure set actually stores the new matrix
    if(nrow(old_l) == nrow(l$get())) {
        stop("set is not saving the new matrix.")
    }
    
    # TRUE because the cache is cleared when a new matrix is set
    if(!is.null(l$getInverse())) { 
        stop("The inverse should be NULL after setting a new matrix.")
    }
}