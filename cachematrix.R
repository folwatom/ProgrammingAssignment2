## This pair of functions cache the inverse of a matrix
## we suppose the input matrix is a square invertible matrix

## function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {
    invX <- NULL
    set <- function(Y) {
        X <<- Y
        invX <<- NULL
    }
    get <- function() X
    setInvM <- function(invM) invX <<- invM
    getInvM <- function() invX
    list(set = set, get = get,
         setInvM = setInvM,
         getInvM = getInvM)
}


## function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'X'
    
    invM <- X$getInvM()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    
    data <- X$get()
    invM <- solve(data, ...)
    X$setInvM(invM)
    
    invM
}
