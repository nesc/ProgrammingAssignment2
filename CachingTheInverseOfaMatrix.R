## makeCacheMatrix: This function creates a matrix 
## Object can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Used to initiate inverse variable
  m <- NULL
  ## set matrix data
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get matrix data
  get <- function() x
  ## set inverse matrix
  setSolve <- function(solve) m <<- solve
  ## get inverse matrix
  getSolve <- function() m
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve: This function have cache inverse matrix.
cacheSolve <- function(x, ...) {
  ## get inverse data from cache
  m <- x$getsolve()
  ## if there have data in the cache, then return cache data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##else, compute the inverse data and save it in the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
}