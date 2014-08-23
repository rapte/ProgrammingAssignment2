# vim: fdm=marker nowrap

## A pair of functions that cache the inverse of a matrix. {{{1


## This function creates a special "matrix" object that can cache its inverse. {{{2

makeCacheMatrix <- function(x = matrix()) {

    # Initialise the inverse variable
    i <- NULL

    # set the matrix
    set <- function(matrix) {
        x <<- matrix
        i <<- NULL
    }

    # get the matrix
    get <- function() {
        x
    }

    # set the inverse of the matrix
    set_inverse <- function(inverse) {
        i <<- inverse
    }

    # get the inverse of the matrix
    get_inverse <- function() {
        i
    }

    # returns the lists created above
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## This function computes the inverse of the special "matrix" returned {{{2
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {

    # Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()

    # retrive the inverse from the cache if the matrix has not changed
    # and the inverse has already been calculated
    if(!is.null(m)) {
        message("getting  cached data")
        return(m)
    }

    # get the matrix
    data <- x$get()

    # solve
    m <- solve(data, ...)

    # set the inverse
    x$set_inverse(m)

    # returns the matrix
    m

}
