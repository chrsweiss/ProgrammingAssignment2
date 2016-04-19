## This set of functions is used to either calculate the inverse
##  of a function or (in order to save system resources) return
##  a cached value for the inverse if it has already been calculated.

## The function makeCacheMatrix returns a list of functions, which is
##  primarily useful for setting the matrix, or returning the inverse
##  (as long as the inverse has already been calculated).

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the inverse of a matrix. If there is a cached value for the
##  matrix it will return cached value, rather than re-calculating.
##  Input here is actually a list (from makeCacheMatrix, after the set
##  function has been called), not a matrix itself.

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
