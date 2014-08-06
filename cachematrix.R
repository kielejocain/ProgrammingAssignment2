## These functions are designed to save a bit of computer time
## by caching the inverse of a matrix so that it does not need
## to be repeatedly computed.

## The first function is used to make a list that contains the
## desired matrix and several methods for it, including
## computation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- matrix()
      set <- function(y) {
            x <<- y
            inv <<- matrix()
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function either computes and stores the inverse
## of the matrix if this hasn't been done, or retrieves the
## inverse without computation if it has.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!identical(inv, matrix())) {
              message("getting cached inverse")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
}