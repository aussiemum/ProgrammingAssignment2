## This program contains two functions which together allow you
# to cache the results of matrix inversion
## the first function makeCacheMatrix creates a matrix which can
## cache its inverse
## the second function cacheSolve calculates the inverse  of the matrix
## if it hasn't been calucating already, otherwise it returns the cached value
##
## This code was largely inspired by the sample code provided for this coursera course:
# https://class.coursera.org/rprog-011/human_grading/view/courses/973492/assessments/3/submissions
##
## # Last modified February 19th, 10:38pm EST
#
# The first function is makeCacheMatrix
# it takes as input a matrix, and produces functions for
# cacheing the input of that matrix
#
# sample input: z = matrix(c(1,2,3,0),nrow=2,ncol=2)
# q = makeCacheMatrix(z)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#### cacheSolve
# function cacheSolve returns the inverse of a matrix x, and caches the result
# it assumes that the matrix supplied is invertible
# if the inverse has been previously calculated, it returns the cached value
# rather than recalculating it
# It only works with a matrix created using makeCacheMatrix
#  sample usage: z = matrix(c(1,2,3,0),nrow=2,ncol=2)
# q = makeCacheMatrix(z)
# cacheSolve(q)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
