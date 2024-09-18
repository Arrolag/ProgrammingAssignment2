## This file contains two functions, makeCacheMatrix and cacheSolve. 

## The function makeCacheMatrix creates an R object that stores a matrix
## and its inverse. It does this using four functions: 
## set, get, setinv, and getinv.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The function cacheSolve calculates the inverse of the special matrix 
## created using the makeCacheMatrix function above. 
## It first checks whether the matrix inverse has already been calculated. If this is
## the case, it retrieves this inverse from the cache and does not compute it.
## Otherwise, it computes the inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i      
}
