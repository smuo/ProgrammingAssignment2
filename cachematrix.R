## makeCacheMatrix: create matrix object
## cacheSolve: get cached inverse matrix

## function makeCacheMatrix()
## Create an object consisting of get, set, getCacheInverse, and
## setCacheInverse functions, by passing in a matrix as input

makeCacheMatrix <- function(x = matrix()) {
  ## set cached inverse to NULL
  i <- NULL
  
  ## set function: set matrix value and reset cached inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get function: return the matrix
  get <- function() x
  
  ## set the cached inverse matrix
  setCacheInverse <- function(inv) i <<- inv
  
  ## get the cached inverse matrix
  getCacheInverse <- function() i
  
  ## return function list
  list(set = set,
       get = get,
       setCacheInverse = setCacheInverse,
       getCacheInverse = getCacheInverse)
}

## function cacheSolve()
## Pass an object created with makeCacheMatrix and return
## the inverse matrix - using the cached inverse if it's
## available, otherwise calculating the inverse and setting
## the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getCacheInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## there is no cached inverse; calculate and set the cache
  i <- solve(x$get())
  x$setCacheInverse(i)
  i
}
