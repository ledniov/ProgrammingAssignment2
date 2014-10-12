## This file contains functions for working with cached inversed matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    set <- function(y) {
      x <<- y
      inversed <<- NULL
    }
    get <- function() x
    
    setInversed <- function(inv) inversed <<- inv
    getInversed <- function() inversed
    
    list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Returns matrix that is the inverse of 'x', matrix created by makeCacheMatrix() function 
## that is able to cache it's inverse

cacheSolve <- function(x, ...) {
    inversedMatrix <- x$getInversed()
  
    if(!is.null(inversedMatrix)) {
      message("getting cached data")
      return(inversedMatrix)
    }
  
    data <- x$get()
    inversedMatrix <- solve(data, ...)
    x$setInversed(inversedMatrix)
  
    inversedMatrix
}