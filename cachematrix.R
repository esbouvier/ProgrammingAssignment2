## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(theMatrix = matrix()) {
    matrixInverse <- NULL
    
    ## Set the value of the matrix and clear its inverse
    set <- function(x) {
        theMatrix <<- x
        matrixInverse <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() {
        theMatrix
    }
    
    ## Set the value of the matrix inverse
    setinverse <- function(solve) {
        matrixInverse <<- solve
    }
    
    ## Get the value of the matrix inverse
    getinverse <- function() {
        matrixInverse
    }
    
    ## Return the "special" matrix i.e. a list of functions to get/set the original 
    ## matrix and its inverse
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Attempt to get the matrix inverse from cache
    matrixInverse <- x$getinverse()
    
    ## If the inverse of the matrix has already been cached then return it
    if (!is.null(matrixInverse)) {
        message("Getting cached data")
        return(matrixInverse)
    }
    
    ## Get data for the matrix to be processed
    data <- x$get()
    
    ## Inverse the matrix
    matrixInverse <- solve(data, ...)
    
    ## Store the matrix inverse in cache
    x$setinverse(matrixInverse)
    
    ## Return the matrix inverse
    matrixInverse
}
