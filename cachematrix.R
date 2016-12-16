## This function calculates the inverse of a matrix, stores the
## result in a cache and returns the result.

## The makeCacheMatrix function stores a list of functions that are used in the next function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
        
}


## The function cacheSolve returns the value of the inverse of the
## matrix, if it is already saved in cache; otherwise, it calculates
## the inverse, returns the value and also saves it in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
