## Creates an object which works with your matrix.
## cacheSolve takes the object created by makeCacheMatrix and
## inverts the matrix, or returns the cached value, if the
## computation has already been done.

## makeCacheMatrix
## Creates an object for cacheSolve to use. Contains four functions
## which do the following:
## set: Establishes a new matrix to prepare inverting and caching.
## get: Returns the matrix you are working with.
## setinverse: Sets the inverse in the parent environment.
## getinverse: Gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get,
         setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve
## Solves for an inverse, or returns the cached value.
## Input the makeCacheMatrix variable into this.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
