## R COURSE PROGRAMMING ASSIGNMENT 2
##
## Two functions that handle a special matrix object
## wich can cache its inverse. 

## makeCacheMatrix: create the special matrix object
## usage: 
##        m<-makeCacheMatrix()   # or 
##        m<-makeCacheMatrix(x)  # create the special matrix
##        m$set(x)               # set the matrix
##        m$get(x)               # get the matrix
##        m$setinv(xinv)         # set the inverse of the matrix
##        m$getinv()             # get the inverse of the matrix
## where x is a matrix object and xinv is the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(xinv) inv <<- xinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: calculate the inverse of a special matrix object
##             or (if exist) return the cached inverse 
## usage: 
##        cacheSolve(m)   
## where m is the special matrix object created with makeCacheMatrix
## when the inverse is cached the function output a message

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}
