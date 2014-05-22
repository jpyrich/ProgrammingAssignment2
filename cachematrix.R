## This function creates a special type of matrix object.  This object can 
## calculate and cache the inverse of a matrix.

## Functions to interact with this object are:

##    set <- function(matrixData)         : Sets the value of the matrix
##    get <- function()                   : Returns the value of the matrix
##    setInverse <- function(inverseData) : Sets the value of the cached matrix inverse
##    getInverse <- function()            : Returns the value of the cached matrix inverse

## These functions are stored in a list and can be accessed using the $ (e.g. x$getInverse())

makeCacheMatrix <- function(cacheMatrix = matrix()) {
    
    ## Initialize inverse to NULL
    cacheInverse <- NULL
    
    ## Define sub-functions
    
    ## set cached matrix
    set <- function(matrixData) {
        cacheMatrix <<- matrixData
        cacheInverse <<- NULL
    }
    
    ## get cached matrix
    get <- function() {
        cacheMatrix
    }
    
    ## set matrix inverse
    setInverse <- function(inverseData) {
        cacheInverse <<- inverseData
    }
    
    ## get matrix inverse
    getInverse <- function() {
        cacheInverse
    }
    
    ## create function list to store functions so that they can be called with $
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function generates the inverse of a matrix and caches the value.  The first time
## the function is called the inverse is calculated and cached.  Subsequent calls to the 
## function will return the cached value.

cacheSolve <- function(cacheMatrix, ...) {
    
    ## Retrieve the cached inverse
    cacheInverse <- cacheMatrix$getInverse()

    ## If cache exists, return the cached value
    if(!is.null(cacheInverse)) {
        message("getting cached data")
        return(cacheInverse)
    }
    
    ## cache does not exist.  Calculate and cache inverse
    message("generating matrix inverse")

    matrixData <- cacheMatrix$get()      ## get the matrix data
    inverseData <- solve(matrixData)     ## generate inverse
    cacheMatrix$setInverse(inverseData)  ## cache the inverse
    return(inverseData)                  ## return the inverse
}