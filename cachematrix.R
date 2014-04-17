# In this exercise we use the `<<-` operator which can be used to assign a value
# to an object in an environment that is different from the current environment.
# Below are two functions that are used to create a special object that stores a
# numeric matrix and cache's its inverse.


# The first function, `makeCacheMatrix` creates a special "matrix", which is 
# really a list containing a function to
# * 1.  set the value of the matrix 
# * 2.  get the value of the matrix 
# * 3.  set the value of the inverse 
# * 4.  get the value of the inverse

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


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (is.null(inv)) {
    inv <- solve(x$get())
    x$setinv(inv)
  }
  
  return(inv)
  
}
