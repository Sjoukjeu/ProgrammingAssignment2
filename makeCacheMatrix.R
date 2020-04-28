## the goal is to write a pair of functions
##'makeCacheMatrix' and 'CacheSolve'
## to cache the inverse of a matrix

##'makeCacheMatrix' creates a matrix that can cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

