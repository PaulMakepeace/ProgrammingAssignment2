## These functions work together to provide a means of caching
## inverted matrices. No error checking is performed for
## uninvertible matrices.

## Example usage:
## myMatrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## myCachedMatrix <- makeCacheMatrix(myMatrix)
## cacheSolve(myCachedMatrix)

## Return a cache for a matrix 'x'.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverseCache <- NULL
    set <- function(y) {
        x <<- y
        inverseCache <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseCache <<- inverse
    getinverse <- function() inverseCache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of cached matrix 'x'
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve returns the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        # uncomment this line for debugging the cache
        # message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

