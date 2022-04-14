# Functions that calculate the inverse of a matrix, or gets it from cache
# if it has already been calculated

# returns an object where matrices can be set or get, as well as is inverse

makeCacheMatrix <- function(x = matrix()) {
    # defines the inverse function as NULL for the first time
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    # list with names so we can access them with $ operator
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# calculates the inverse of a matrix if the inverse is not already stored
# in cache

cacheSolve <- function(x, ...) {
    # return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    # if inv exists, then get cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # gets existing data
    data <- x$get()
    # calculates inverse of data matrix
    inv <- solve(data, ...)
    # sets inverse and returns
    x$setinverse(inv)
    inv
}
