##
## Cache for time-consuming operation of matrix inverse
##
## Coursera DataScience course
## R Programming class
## Assignment #2
## @a : yuriyo
##
## @note : "For this assignment, assume that the matrix supplied is always invertible"

#
## build matrix object that can store inverse for it
#
makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  m_data <- x
  
  get <- function() m_data
  set <- function(x) {
    m_data <<- x
    m_inverse <<- NULL
  }
  
  getinverse <- function() m_inverse
  setinverse <- function(inverse) m_inverse <<- inverse
    
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#
## Calculate inverse of wrapped matrix object
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #
  # get the cached value
  #
  inverse <- x$getinverse()
  #
  # that would be empty not calculated yet
  #
  if(!is.null(inverse)) {
    return(inverse)
  }
  #
  # calculate inverse
  #
  inverse <- solve(x$get(), ...)
  #
  # push it as cache value
  #
  x$setinverse(inverse)
  #
  # result
  #
  inverse
}
