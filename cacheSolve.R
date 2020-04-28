
## 'cacheSolve' computes the inverse of a matrix
## returned by 'makeCacheMatrix'.
## If inverse has been calculated, cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  #if the inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if the inverse has not been calculated yet
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
