## Here we have two functions which are responsible for getting,
## setting and calculating the inverse of a matrix

## makeCacheMatrix function is responsible for creating the cache objects
## setting and getting the original matrix.
## setting and getting the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function is responsible for checking for the 
## invert of the input matrix in the cache if so it returns the invert 
## else it calculates the invert and saves is to the cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
