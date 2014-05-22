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
  #
  # fields in function scope
  #
  m_inverse <- NULL
  m_data <- x
  
  # get stored value of original matrix
  get <- function() m_data

  # on new matrix, clear cached value and ressign matrix field
  set <- function(x) {
    m_data <<- x
    m_inverse <<- NULL
  }
  
  # provide cached value
  getinverse <- function() m_inverse
  # reset cached value of inverse matrix. No check of actually it's inverted of original
  setinverse <- function(inverse) m_inverse <<- inverse
    
  # return methods of matrics wrapper
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
