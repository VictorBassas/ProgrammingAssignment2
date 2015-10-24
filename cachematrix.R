## This file contains two functions makeCacheMatrix and cacheSolve.
## Function makeCacheMatrix(): creates a special "matrix" that the function
## cacheSolve() can use later

## Write a short comment describing this function
## makeCacheMatrix description
## x is a square invertible matrix
## returns a list with functions:
##  set = sets the matrix
##  get = gets the matrix
##  setinv = sets the inverse of the matrix
##  getinv = gets the inverse of the matrix
## The matrix and this list will be used by the cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set = function (y){
    # I'm using <<- to assign a value to an object in an environment 
    # that is different from the current environment
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function () inv
  list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## cacheSolve() calculates the inverse of the matrix returned by 
## makeCacheMatrix() shown above. However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse
## from the cache and does not calculate it again. If it is not cached, 
## then cacheSolve() calculates the inverse and sets the value in the cache 
## through the setinv function



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if the inverse has been already calculated
  if (!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
  }
  # if not, then calculate the inverse
  data <- x$get()
  inv <- solve(data,...)
  #set the value of the inverse in cache by calling setinv function
  x$setinv(inv)
  return (inv)
}

