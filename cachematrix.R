## Programming Assignment 2: Lexical Scoping
##
## In this assignment we use the <<- operator which can be used to assign 
## a value to an object in an environment that is different from the current 
## environment. makeCacheMatrix and cacheSolve are two functions that 
## create a special object that stores a numeric matrix 
## and cache's its inverse.

## makeCacheMatrix: creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  m_i <- NULL
  set <- function(y) {
    m <<- y
    m_i <<- NULL   # Clear the inverse when the matrix changes.
  }
  get <- function() m
  setinverse <- function(y) m_1 <<- y
  getinverse <- function() m_1
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## return the cached inverse. Otherwise solve for the inverse, 
## cache the solution, and return the result.
##
## For this assignment we assume that the matrix supplied is
## always invertible.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of m
  m_i <- m$getinverse()
  
  ## If m_i is not null we have a cached solution.
  if(!is.null(m_i)) {
    message("Using cached result.")
    return(m_i)
  }
  
  # Otherwise m_i is null and needs to be solved.
  data <- m$get()
  m_i <- solve(data, ...)    
  m$setinverse(m_i)
  m_i
}
