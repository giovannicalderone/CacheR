## Coursera Data Science: R Programming Week 3 Assignment


## The function makeCacheMatrix set the value of the input matrix,
## get its value, set the inverse matrix and get its inverse.

makeCacheMatrix <- function(x = matrix()) {

    invMatrix <- NULL 
    
    setMatrix <- function(y) {                 
        x <<- y                           
        invMatrix <<- NULL                      
    }
    
    getMatrix <- function() x
        
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)  

}


## The function cacheSolve computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
    
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)) {
        message("Retrieving the cached Matrix")
        return(invMatrix)
    }
    
    #else:
    inputMatrix <- x$getMatrix()
    inv <- solve(inputMatrix, ...)
    x$setInverse(inv)
    inv
}