## makeCacheMatrix and cacheSolve are two functions that solve the inverse 
## of an invertible matrix, store the inverse in cache and 
## return the cached value when the inverse is requested again.
## By doing this we are eliminating the need to process this task
## multiple times.

## makeCacheMatrix creates a list of functions which stores 
## the inverse of a matrix in cache so in the next attempt 
## to caltulate, the previously saved value is returned. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks if the inverse of a requested matrix
## is stored in cache and retrieves it
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
