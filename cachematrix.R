## These functions provide for caching of the inverse of a matrix.
## When makeCacheMatrix is first called, the passed matrix (or default) is stored, and inverse cache cleared.
## The first time cacheSolve is called after that, the cache is empty, so the inverse is calculated, and stored in the cache. 
## All subsequent calls to cacheSolve with the same matrix will then return the cached value, without requiring it to be recalculated. 

## Store the passed (or default) matrix and clear the cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return the inverse of the passed cacheMatrix. 
## If it has been previously calculated and cached, return the cached inverse. 
## If this is the first time cacheSolve has been called for this cacheMatrix, calculate the inverse, store in the cache, 
## and then return the cached inverse. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
