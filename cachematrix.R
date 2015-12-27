## The first function makeCacheMatrix creates a list containing a function to set 
## and get the value, set and get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This second function calculates the inverse of the matrix from the list 
## created in the first function
## It checks if the inverse had been already calculated, if yes it takes the 
## result from the cache and skip the computation

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
