## Here we have two functions which are responsible for getting,
## setting and calculating the inverse of a matrix


## makeCacheMatrix function is responsible for creating the cache objects
## setting and getting the original matrix.
## setting and getting the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ##Initialize inverse with null
    set <- function(y) { ## Initialize setter function
      x <<- y
      inverse <<- NULL
    }
    get <- function() x ## Initialize getter function
    setInverse <- function(inv) inverse <<- inv ## Initialize set Inverse function
    getInverse <- function() inverse## Initialize get inverse function
    list(set = set, get = get,## return list of functions in the object
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function is responsible for checking for the 
## invert of the input matrix in the cache if so it returns the invert 
## else it calculates the invert and saves is to the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() ## get cached inverse
  if(!is.null(inverse)) {## check if there is a cached inverse
    message("getting cached inverse matrix")
    return(inverse) ## return cached inverse if exist
  }
  data <- x$get()## get original matrix
  inverse <- solve(data)## calculate inverse for the matrix
  x$setInverse(inverse)## save inverse to cache
  inverse ## return the calculated inverse
}
