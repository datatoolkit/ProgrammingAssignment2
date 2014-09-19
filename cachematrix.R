## The pair of functions makeCacheMatrix() and cacheSolve() create
## a setup for saving computation time on repeatedly inverting a matrices.
## It works for square invertible matrices

## Function makeCacheMatrix() takes a matrix, stores it in its environment,
## and returns a list of functions. These functions are:
## get() - returns the matrix
## set() - set the matrix to a new value, and nulls the cached inverse
## getinverse() - returns the inverse of the matrix. if the inverse was
##                not already calculated it caches the inverse matrix
##                before returning it. If it is already cached then the
##                the cached value is returned.
makeCacheMatrix <- function(x = matrix()) {

  ## when m is null then the inverse was not yet calculated
  m <- NULL
  ## set the value of 'set' to a function that sets the value and nulls m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## set the value of 'get' to a function that returns the matrix
  get <- function() x
  ## set the value of 'setinverse' to a function that calculates the inverse
  setinverse <- function(solve) m <<- solve
  ## set the value of 'setinverse' to a function that returns the inverse
  getinverse <- function() m
  ## return a list of functions that can address the internal cached variables
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve() takes a list returned by makeCacheMatrix() and
## returns the inverse of the matrix that was used to call makeCacheMatrix()
## The trick is that if m == null it means the inverse had never been
## calculated, and this it sends the function 'setinverse' to look for the
## matrix stored in makeCacheMatrix() and compute the inverse.  If it was
## already computed it retuns it with a proud message that it was cached...
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if m is not null then we can returned the cached vlaue
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if m is null than we first retrieved the matrix, then we set m
  ## to the inverse, and then we set 'setinverse' to that inverse so it
  ## can be retrieved
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

