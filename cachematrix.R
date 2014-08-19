## Creates a list of functions that
##
## sets a matrix (setmatrix)
## gets a matrix (getmatrix)
## sets the inverse of the matrix (setinverse)
## gets the inverse of the matrix (getinverse)
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)

}


## Returns the inverse of a matrix. If the inverse has already been calculated, the function returns the cached value
##
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
