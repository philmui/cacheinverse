##
## Create a matrix cache-able inverse set of functions
##
## Assume: assume that the matrix supplied is always invertible
## Date: Sat Feb 6 20:08:14 PST 2016
##

##
## This function creates a special "matrix" object 
## that can cache its inverse.
##
## Assume: assume that the matrix supplied is always invertible
##

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
}

##
## This function returns a matrix that is the 
##      inverse of 'x'
## It computes the inverse of the special 
##     "matrix" returned by makeCacheMatrix above
##
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
