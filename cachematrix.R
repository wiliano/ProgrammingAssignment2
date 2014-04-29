## These functions compute the inverse of a square matrix
## taking into account caching. The first time a cached
## matrix is used for the calculation of it's inverse, it
## is computed and stored in cache. Subsequent calls will
## return the cached version.

## makeCacheMatrix creates a cacheable matrix specifically used for inverse
## computations. It contains a list of functions that get/set
## the value of the matrix, and get/set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of a cacheable matrix created by
## the function makeCacheMatrix. If the value has been already computed,
## the cached result is returned. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
