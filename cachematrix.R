## Assignment 2: Caching the Inverse of a Matrix
#

#setwd("ProgrammingAssignment2")

# makeCacheMatrix: This function creates a special "matrix" object 
#                  that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
#             returned by makeCacheMatrix above. If the inverse has 
#             already been calculated (and the matrix has not changed), 
#             then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## Test basic caching
m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()

inv <- cacheSolve(x)
m %*% inv
inv

#3
n <- 3
mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
matCached <- makeCacheMatrix(mat)
matSolved1 <- cacheSolve(matCached)
matSolved2 <- cacheSolve(matCached)
if (!identical(matSolved1, matSolved2))
    message("Cached version does not match solved version")

## Use a time test to see if we really save time
##
n <- 128
mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
matCached <- makeCacheMatrix(mat)
time1 <- system.time(matSolved1 <- cacheSolve(matCached))
time2 <- system.time(matSolved2 <- cacheSolve(matCached))
if (time1["user.self"] < time2["user.self"])
    message("Solve time is less than cache time")

