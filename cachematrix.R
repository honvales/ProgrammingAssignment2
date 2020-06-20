## These functions compute the inverse of a matrix in a
## computationally efficient manner. makeCacheMatrix
## calculates the inverse of a matrix and stores it in memory.
## cacheSolve checks if the inverse has been computed and
## retrieves the inverse from memory. If the inverse is not
## stored in memory, the function computes the inverse and
## stores it. This code is a modified version of the example
## described in the assignment with additional annotations

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Set value of matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        # Get value of matrix
        get <- function() x
        # Set inverse of matrix
        setinv <- function(solve) m <<- solve
        # Get inverse of mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function determines if the inverse matrix of x
## is stored in memory. If it is, then the function returns
## the stored inverse. If it is not, then the function computes
## the inverse, stores it in memory, and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        ## Is the inverse stored in memory?
        ## Return if it is
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Calculate inverse if it is not stored
        m <- solve(data, ...)
        x$setinv(m)
        m
}
