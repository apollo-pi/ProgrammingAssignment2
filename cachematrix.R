## The first function in the file, makeCacheMatrix() creates an R object that
## stores a matrix and its inverse. The second function, cacheSolve() requires
## an argument that is returned by makeCacheMatrix() in order to retrieve the
## mean from the cached value that is stored in the makeCacheMatrix() object's
## environment.
## The object that makeCacheMatrix() creates contains four functions:
## - set(): sets the argument as the value of the matrix stored.
## - get(): gets the value of the matrix stored.
## - setinv(): sets the argument as the value of the inverse of the matrix stored.
## - getinv(): gets the value of the inverse of the matrix stored.
## cacheSolve() first checks if the inverse has already been calculated and
## stored with getinv(). If so, it returns the matrix stored. If not, it
## calculates it and stores the inverse in the object.

## An example of how this works:
## > mat <- makeCacheMatrix(matrix(1:4,2,2))  # Creates the object inside mat.
##
## > mat$get()                                # Retrieves the value of the matrix stored in mat.
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > mat$getinv()                             # There is no inverse stored in mat.
## NULL
##
## > cacheSolve(mat)                          # Calculates the inverse and stores it in mat.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(mat)                          # If we call the function again, it retrieves the value stored instead of calculating it again.
## Getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## This function builds a set of functions (set, get, setinv, getinv) and
## returns the functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function is required to populate and/or retrieve the inverse from an
## object of type makeCacheMatrix().

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}