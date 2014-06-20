## Functions to create a square matrix and calculate its inverse.
## Assumes only a solvable e.g. square matrix as input.

## Function creates a matrix object that can cache its inverse.  Object created to
## be available to functions outside of immediate environment.
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Determines if inverse of matrix has been solved; returns cached version
## or solves as appropriate.  
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
