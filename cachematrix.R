## makeCacheMatrix creates a special matrix obj. then cacheSolve calculates the inverse

## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) invX <<- inv
    getInverse <- function() invX
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    invX <- x$getInverse()
    if(!is.null(invX)) { # gets the inverse matrix if it has been already calculated
        message("Getting cached matrix")
        return(invX)
    } 
    data <- x$get() # matrix from makeCacheMatrix
    invX <- solve(data) # used to calculate the inverse
    x$setInverse(invX)
    invX
}
##End
