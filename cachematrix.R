# Returns the inverse of a matrix using cacheSolve
# The cacheSolve will also store a copy of the cached matrix
# so this does not need to be recomputed for future calls.

# example usage:
#> m <- matrix(c(2, 3, 2, 2), nrow=2, ncol=2)
#> cacheMatrix <- makeCacheMatrix(m)
#> cacheSolve(cacheMatrix)



## Return a list of functions to operate on a special Matrix
## set,get for updating the matrix
## setinverse,getinverse for storing the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    cm <- NULL
    
    # Sets the matrix and clears the cache
    set <- function(y) {
        x <<- y
        cm <<- NULL 
    }
    
    # Returns the matrix  
    get <- function() { x }
    
    # Sets the cached inverse of the matrix
    setinverse <- function(inverse) { cm <<- inverse }
    
    # Returns the cached inverse of the matrix
    # This is null unless setinverse has been called
    getinverse <- function() { cm }
    
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}

## Return a matrix that is the inverse of cacheMatrix 'x'
## If x has already been solved then we use the cached copy
cacheSolve <- function(x, ...) {
    
    # Checks and returns cached copy if one exists
    cm <- x$getinverse()
    if(!is.null(cm)) {
        message("Using cached copy.")
        return(cm)        
    }
    
    # Solves and caches the matrix
    data <- x$get()
    cm <- solve(data)
    x$setinverse(cm)
    
    cm
}
