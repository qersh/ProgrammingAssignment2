## This script is to calculate and caching the inverse of a matrix 
## rather than compute it repeatedly this script contain 2 functions
## makeCacheMatrix() and cacheSolve() 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Init z with NULL value
    z <- NULL
    
    # When calling "set" mackCaheMatrix "x" value to be like "y"
    # and set "z" value to NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    
    # return "x" value when called
    get <- function() x
    # set the inverse of the matrix using solve 
    setInverse <- function(solve) z <<- solve
    # return the inverse value stored in "z"
    getInverse <- function() z
    
    # List of functions in this function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # call getInverse Function and assign the value to "z"
    z <- x$getInverse()
    # check if the Inverse already set
    if(!is.null(z)) {
        # if yes write a message and return "z"
        message("getting cached data")
        return(z)
    }
    # get the value and assign to data
    data <- x$get()
    # use solve to Inverse the matrix
    z <- solve(data, ...)
    # call the function to set the Inverse value (to use it later)
    x$setInverse(z)
    # return the inverse value 
    z
}
