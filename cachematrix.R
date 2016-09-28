## Assignment to write a pair of functions that cache the inverse of a matrix

## Function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
        set <- function (y) {
                x <<- y
                invr <<- NULL
        }
        get <- function() x
        setinvr <- function(solve) invr <<- solve
        getinvr <- function() invr
        list(set = set, get = get, setinvr = setinvr, getinvr = getinvr)
}


## Function computes the inverst of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        invr <- x$getinvr()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinvr(invr)
        invr
        ## Return a matrix that is the inverse of 'x'
}
