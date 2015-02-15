## Matrix inversion is usually a costly computation,
## so there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly.
## The following functions implement the matrix inverse caching mechanism

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse

  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
