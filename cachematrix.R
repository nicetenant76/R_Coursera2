## Demonstrates how to leverage lexical scoping in R to have manageable mutable
## state without using global variables directly.
##
## A builder function (makeCacheMatrix) creates an environment that is preserved
## after the function exits and is used to cache values safely for the lifetime of the 
## enclosing scope.


# provides environment for matrix and cached inverse value
# returns list of closures for access
makeCacheMatrix <- function(m = matrix()) {
    cachedInverse <- NULL
    set <- function(newMatrix) {
        m <<- newMatrix
        cachedInverse <<- NULL # wipe cache on change
    }
    get <- function() m
    setInverse <- function(newInverse) cachedInverse <<- newInverse
    getInverse <- function() cachedInverse

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


# uses the passed in makeCacheMatrix object (x) to either return a cached inverse 
# of its matrix or compute it once for subsequent cached access
cacheSolve <- function(x, ...) {
        im <- x$getInverse()
        if (!is.null(im)) {
            message("getting cached inverse")
            return(im)
        }
        # inverse has not been computed yet
        x$setInverse(solve(x$get()))
        x$getInverse()
}