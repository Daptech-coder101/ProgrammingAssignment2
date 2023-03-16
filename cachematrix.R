## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly. The goal here is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#   1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#   2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
#      the inverse from the cache.
# 
# For this objective, assume that the matrix supplied is always invertible.


## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
# create a function that returns a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to NULL
  inv <- NULL
  
  # define the 'set' function that sets the value of the matrix and clears the cache
  set <- function(y) {
    # use the '<<-' operator to assign 'y' to the 'x' variable in the global environment
    x <<- y
    # clear the cached inverse by setting it to NULL
    inv <<- NULL
  }
  
  # define the 'get' function that returns the value of the matrix
  get <- function() x
  
  # define the 'setinv' function that sets the value of the cached inverse
  setinv <- function(inverse) inv <<- inverse
  
  # define the 'getinv' function that returns the cached inverse
  getinv <- function() inv
  
  # return a list containing the 'set', 'get', 'setinv', and 'getinv' functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# create a function that calculates the inverse of a special "matrix" object and caches the result
cacheSolve <- function(x, ...) {
  # get the value of the matrix using the 'get' function
  m <- x$get()
  
  # get the cached inverse using the 'getinv' function
  inv <- x$getinv()
  
  # if the cached inverse is not NULL, return it and print a message indicating that the cached data is being used
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the cached inverse is NULL, calculate the inverse of the matrix using the 'solve' function
  inv <- solve(m, ...)
  
  # set the cached inverse using the 'setinv' function
  x$setinv(inv)
  
  # print a message indicating that the inverse is being calculated
  message("calculating inverse")
  
  # return the calculated inverse
  inv
}


