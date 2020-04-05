## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix holds the code for the 4 functions that put the matrices
## and their inverses into the Cache. This function and the 4 functions within are written
## to allow the function cacheSolve to run more efficiently by caching inversions already
## performed.

## the function x$set adds the pair x, inverse into the cache with the value x and NULL
## the function x$get 
## the function x$setinverted updates the chache inverse value associated with x to the proper inverse
## the function x$getinverted pulls the inverse value associated with x from the cache

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverted <- function(solution) inverse <<- solution
    getinverted <- function() inverse
    list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}
 
 
## Write a short comment describing this function
## cacheSolve is a function that inverts a square matix called x
## this function tries to be efficient by using the cache. Every time a matrix is inverted,
## the solution is stored in the cache. Before inverting a new matrix, the program looks in
## the cache to see if it is already there and pulls it from the cache if it is there.
## If the inverse is not there, it adds it to the cache.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverted()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverted(inverse)
    inverse
}