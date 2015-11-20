## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a special "matrix", which is really a list containing a 
#function to
#1.- setMatrix: to set the value of the matrix
#2.- getMatrix: to get the value of the matrix
#3.- setInverse: to set the value of the inverse
#4.- getInverse: to get the value of the inverse
makeCacheMatrix <- function(m = matrix()) {
  #internal variable to save the inverse of a matrix.
  inv <- NULL
  
  #set matrix function to set a new Matrix
  setMatrix <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  #get matrix function to get the Matrix
  getMatrix <- function() m
  
  #set the Inverse of the function
  setInverse <- function(inversa) inv <<- inversa
  
  #get the Inverse of the function
  getInverse <- function() inv
  
  #return a List of the function
  list(setmatrix = setMatrix, getmatrix = getMatrix,
       setinverse = setInverse,
       getinverse = getInverse)
}


## Write a short comment describing this function
#Given a makeCacheMatrix object, 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinverse()
  
  if( !is.null(inversa) ) {
    message("Getting cached data")
    return(inversa)
  }
  
  m <- x$getmatrix()
  inversa <- solve(m, ...)
  x$setinverse(inversa)
  inversa
}