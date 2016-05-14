## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix is a function which creates a speicial "matrix" object that can cache its inverse.
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
#setMatrix		set the value of the matrix
#getMatrix		get the value of the matrix
#setInverse		set the cached value (inverse of the matrix) 
#getInverse		get the cached value (inverse of the matrix)
#Note: <<- operator which can be used to assign a value to an object in an environment that is 
#different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  #inv variable holds the cached value or NULL if nothing is cached.
  #Initially, nothing is cached, hence set it to NULL
  inv <- NULL
  
  #setMatrix stores a matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #getMatrix returs the stored matrix
  getMatrix <- function() x
  
  #setInverse caches a given argument
  setInverse <- function(inverse) inv <<- inverse
  
  #getInverse gets the cached value
  getInverse <- function() inv
  
  #Return x list
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If 
#the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if 
#X is a square invertible matrix, then solve(X) returns its inverse.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if 
#X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$getMatrix()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

