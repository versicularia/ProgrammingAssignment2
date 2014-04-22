## These two functions allow caching the inverse of a matrix
## to avoid calculating it anew each time it is required

## The following function creates a special "matrix" that can cache its own inverse
## It is actually a list containing 4 functions to set and get the values of
## the matrix and of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function inverts the special "matrix" created with the 
## above function, but first it checks to see if the inverse has already been 
## computed. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it inverts the matrix and caches the result via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}