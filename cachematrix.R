## Put comments here that give an overall description of what your
## functions do

## Creates a list of functions to store the state and cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of the special matrix defined above, but only recalculates if it hasn't been cached

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
