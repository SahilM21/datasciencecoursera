## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  invrs <- NULL
  
  ## get/set for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  
  ## get/set for matrix inverse
  getinvrs <- function() invrs
  setinvrs <- function(inverse) invrs <<- inverse
  
  ## return list of functions for matrix
  list(get=get, set=set, getinvrs=getinvrs, setinvrs=setinvrs)
}

# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
#
# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns:
#   The inverse of the matrix
cacheSolve <- function(x, ...) {
  invrs <- x$getinvrs()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(invrs)) {
    message("inverse is cached")
    return(invrs)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  invrs <- solve(m, ...)
  
  # cache inverse
  x$setinvrs(invrs)
  
  # return inverse of matrix
  return(invrs)
}
