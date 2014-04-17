## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMat) {
    x <<- newMat
    inv <<- NULL
  }
  
  get <- function() x
  
  getinv <- function() inv
  setinv <- function(newInv) {
    inv <<- newInv
  }
  
  return(list(set=set, get=get, setinv=setinv, getinv=getinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (is.null(inv)) {
    inv <- solve(x$get())
    x$setinv(inv)
  }
  
  return(inv)
  
}
