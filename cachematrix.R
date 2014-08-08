## These functions are designed to save a bit of computer time
## by caching the inverse of a matrix so that it does not need
## to be repeatedly computed.

## The first function is used to make a list that contains the
## desired matrix and several methods for it, including
## computation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## set the inverse to an empty matrix
      inv <- matrix()
      ## a function to set the matrix anew
      set <- function(y) {
            x <<- y
            inv <<- matrix()
      }
      ## a function to retrieve the original matrix
      get <- function() x
      ## a function to set the inverse matrix
      setinv <- function(solve) inv <<- solve
      ## a function to retrieve the inverse
      getinv <- function() inv
      ## return the list of functions
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function either computes and stores the inverse
## of the matrix if this hasn't been done, or retrieves the
## inverse without computation if it has.

cacheSolve <- function(x, ...) {
      ## collect the inverse matrix
      inv <- x$getinv()
      ## if non-trivial, return the collected inverse matrix
      if(!identical(inv, matrix())) {
            message("getting cached inverse")
            return(inv)
      }
      ## otherwise, collect the original matrix
      data <- x$get()
      ## compute its inverse
      inv <- solve(data, ...)
      ## return the inverse and cache for later retrieval
      x$setinv(inv)
}