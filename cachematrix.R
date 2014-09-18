## These functions enable one to cache the computation of the inverse
## of a matrix. Using the makeCacheMatrix function one can create an
## object which is capable of storing the computation of the inverse.
## The function cacheSolve returns the cached computation or computes
## the inverse if this hasn't been done already.

## This function creates an object which is capable of caching
## the computaiton of the inverse of the matrix given as an argument.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cache solve returns the inverse of a matrix that is 
## stored in a makeCacheMatrix object. If the inverse
## has been calculated already the function will return
## the cached value.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
