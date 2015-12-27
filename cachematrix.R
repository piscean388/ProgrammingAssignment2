## These functions are used to cache inverses of matrices
## in order to make the inverse calculation less expensive
## for very complex matrices

## This function takes a matrix, x as input and creates a 
## custom matrix object for x which caches 
## the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}

## This function takes a custom matrix object created by function
## makeCacheMatrix() above and returns its inverse. If the inverse
## has already been computed and cached, the inverse value is obtained
## from the cache. If not, it is computed using the solve() function.

cacheSolve <- function(x, ...) {
    
    ## Check cache of 'x' to see if inverse has been calculated already
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## Inverse value for x has not been cached. 
    ## Hence, compute and cache the inverse value.
    dataMatrix <- x$get()
    inv <- solve(dataMatrix, ...)
    x$setInverse(inv)
    inv
}
