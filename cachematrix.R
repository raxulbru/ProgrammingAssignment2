## The two functions below are used to create a special
## object that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a “matrix”, which
## is really a list containing a function to:
##    1) set the value of the matrix
##    2) get the value of the matrix
##    3) set the value of the inverse
##    4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
               x <<- y
               inv <<- NULL
           }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve computes the inverse of the “matrix” 
## returned by makeCacheMatrix or retrieves it from
## the cache if the matrix was previously computed.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
