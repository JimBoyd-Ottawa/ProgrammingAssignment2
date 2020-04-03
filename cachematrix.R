## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverted <- function(solve) inverse <<- solve
    getinverted <- function() inverse
    list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverted()
  if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverted
  inverse
}
