## Put comments here that give an overall description of what your
## functions do

## This creates a vector of which is a list that houses a function that sets the value of the vector, then gets
## the value of the vector
## setinversematrix assigns a value to an object in an environment that is different from its current environment

## Write a short comment describing this function

##the function below constructs a special object that stores a matrix that is capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverseofmatrix <- function(inverseofmatrix) i <<- inverseofmatrix
  getinverseofmatrix <- function() i
  list(set = set ,get = get,
       setinverseofmatrix = setinverseofmatrix,
       getinverseofmatrix = getinverseofmatrix)
}
## Write a short comment describing this function
##The below function computes the inverse of a square matrix utilizing the solve function. If the inverse has been
##calculated and the matrix is the same it will return the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverseofmatrix()
  if(!is.null(i)) {
    message("get cached data")
    return(i)
  }
  a  <- x$get()
  i <- solve(a, ...)
  x$setinverseofmatrix(i)
  i
}
