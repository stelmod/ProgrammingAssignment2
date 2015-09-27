## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions create a struct that can cache the inverse of a matrix
## and return it if it has already been calculated.


## This function creates a special "matrix" object that can cache its inverse.
## It supports the following operations:
# Set the value of the matrix
# Get the value of the matrix
# Set the inverse of the matrix
# Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Note: Matrix is assumed to be inversible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  m <- x$get()
  inverse <- solve(m)
  x$set_inverse(inverse)
  inverse
}
