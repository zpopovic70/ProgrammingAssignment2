## Functions to create object which caches matrix and its inverse

## Function makeCacheMatirx takes a matrix as an argument and
### creates an object that caches passed matrix data and its inverse.
### Access to the underlying matrix is provided via get/set methods
### Access to the inverse matrix is provided via getinv/setinv methods

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve takes a CacheMatrix object created with
## the function above and returns the inverse of the matrix stored 
## within the object. Rather than calculating inverse function first
## attempts to get the inverse from the cache. If the inverse of the 
## matrix is not in the cache, it will be calculated and stored to 
## the cache, prior to be returned. If the inverse is presnet in the 
## cache the message will be printed before it gets returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if (!is.null(i)) {
            message("getting cached data")
            return (i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
