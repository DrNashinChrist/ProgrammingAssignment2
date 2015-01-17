## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function creates a special invertaible matrix,
# which contains a list of 4 functions
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
# The function calculates the inverse of the special matrix from above function.
# First, it will check that the matrix is already calculated its inverse.
# If so, it will get the inverse from the cache and skips the calculation.
# If not, it will calculate the inverse of data and
# sets the value of the inverse into the cache via the setinv function.
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

