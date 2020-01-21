## Function makeCacheMatrix creates an object storing a matrix and
## its inverse
## Function cacheSolve gets the inverse of a matrix stores in 
## object makeCacheMatrix, if inverse is not available it stores
## the inverse and returns the newly calculated inverse

## Function makeCacheMatrix creates an object that stores a matrix
## and its inverse

## inputs x has to be an invertible numeric matrix

## set: assigns a new matrix to the object
## get: return the stored matrix
## setInverse: set the inverse of the matrix
## getInverse: get the stored inverse, NULL if not available 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(matInverse) inv <<- matInverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function cacheSolve returns the stored inverse of object
## makeCacheMatrix or calculates the matrix is not yet available
## and stores the value and returns it

## x: object makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
