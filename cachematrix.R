## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    # function to set/reset the matrix data
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get matrix
    get <- function() { x }
    
    #set the inverse matrix
    setInverse <- function(inv) { m <<- inv }
    
    #get the inverse matrix from variable m
    getInverse <- function() { m }
    
    #return list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # get matrix inverse
    m <- x$getInverse()
    
    # if inverse matrix is already present (in m), return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get matrix
    data <- x$get()
    
    #create the inverse
    m <- solve(data, ...)
    
    #set the inverse
    x$setInverse(m)
    
    #return inverse
    m    
}
