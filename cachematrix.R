## Put comments here that give an overall description of what your
## functions do
## The functions below allow a user to create a 'matrix object' (first function) which can then be used to store/cache the inverse of the 
## matrix (second function).
## This removes the need for the inverse to be calculated more than once.
## NOTE: The object needs to be created by running makeCacheMatrix(m1) before cacheSolve can be called. This is because otherwise
## the matrix is not a type of the matrix object and an error will be thrown.

## Write a short comment describing this function
## makeCacheMatrix creates a matrix object which allows you to get and set the matrix, and get and set the inverse matrix.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  # Set matrix
  set <- function(y) {
    x <<- y
    ##set inverseMatrix to null in case the matrix has changed since it was last set
    inverseMatrix <<- NULL
  }
  # Get matrix
  get <- function() x
  # Set inverse of matrix
  setinverse <- function(solvedMatrix) inverseMatrix <<- solvedMatrix
  # Get inverse of matrix
  getinverse <- function() inverseMatrix
  ## The functions that can be called in this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix object. It will check if the inverse has already been calculated by calling getinverse
## and checking if the returned value is null. If the value is null then the inverse has not yet been cached, and setinverse will be
## called. The user can tell if the inverse had been cached because a message is displayed in this situation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the inverse from the object
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    # Inverse has been set, return this
    message("getting cached data")
    return(inverse)
  }
  # Inverse has not been set, calculate, set and return new value
  # Get matrix
  data <- x$get()
  # Invert it
  inverse <- solve(data, ...)
  # Set inverse
  x$setinverse(inverse)
  # Return inverse
  inverse
}
