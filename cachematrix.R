## R functions that are able to cache potentially time-consuming computations
## makeCacheMatrix: creates the matrix objects
## cacheSolve: calculates and stores the inverse into the makeCacheMatrix object

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL

  set <- function(y) {
    if(!identical(y,x))
    {
      x <<- y
      s <<- NULL
    }
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the matrix object returned by makeCacheMatrix
## using the cached one if its available

cacheSolve <- function(x, ...) {

  s <- x$getSolve()

  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)

  s
}
