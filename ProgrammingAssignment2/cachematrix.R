# This program demonstrates the lexical scoping in R
#  The two functions below are used to cache the inverse of a matrix 'x'.
# makeCacheMatrix creates a list containing a function to
#  set the value of the matrix
#  get the value of the matrix
#  set the value of inverse of the matrix
#  get the value of inverse of the matrix
# Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setMatrix <- function(solve) m <<- solve
    getMatrix <- function() m
    list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}
## the cacheSolve function returns the inverse of matrix "x" as "m" 
## if m exist in cache, or computes and return the inverse "m" if it does not 
## exist in cache.  


cacheSolve <- function(x, ...) {
    m <- x$getMatrix()
    if(!is.null(m)) {
    message("Data is in cache, getting inverse of the matrix")
    return(m)
    }
    else{
    message("Data is not in cache, computing inverse of the matrix")
    data <- x$get()     
    m <- solve(data)
    x$setMatrix(m)
    m
    }
}

# Sample output
# > x <- matrix(8:11, nrow=2)
# > m <- makeCacheMatrix(x)
# > 
# > m$get()
#      [,1] [,2]
# [1,]    8   10
# [2,]    9   11
# > 
# > cacheSolve(m)
# Data is not in cache, computing inverse of the matrix
#      [,1] [,2]
# [1,] -5.5    5
# [2,]  4.5   -4
# > 
# > cacheSolve(m)
# Data is in cache, getting inverse of the matrix
#      [,1] [,2]
# [1,] -5.5    5
# [2,]  4.5   -4