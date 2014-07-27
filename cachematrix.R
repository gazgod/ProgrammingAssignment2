## Coursera R programming Assignment 2 CacheMatrix
## 16/07/2014
## By GazGod

## An R Function that can cache potentially time consuming computations
## in this case cache the inverse of a matrix so the cached value can be returned instead of recalculation.


## create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                              # Initialize inverse matrix as NULL
        set <- function(y) {                   # Set matrix new matrix value
                x <<- y                        # Get value of y from parent environments
                m <<- NULL                     # Reset inverse matrix when input matrix is changed
        }
        get <- function() x                    # get the matrix value
        setsolve <- function(solve) m <<- solve  # set the matrix inversion
        getsolve <- function() m               # get the matrix inversion
        list(set = set,                        # list of functions
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Function to compute the matrix inversion
## If already calculated return the cached inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {                      # if inversion already cached return it
                message("getting cached data") 
                return(m)                      # return cached inversion
        }
        data <- x$get()                        # assign data equal to the source matrix
        m <- solve(data)                       # calculate the inversion
        x$setsolve(m)                          # cache the inversion
        m                                      # return calculated inversion
}
