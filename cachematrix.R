## Put comments here that give an overall description of what your
## functions do

## This function is a list of functions that takes that takes a matrix as an argument
# and stores caches the inverse.

makeCacheMatrix <- function(x = as.matrix()){
  inv <- NULL
  set <- function(y = as.matrix()){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv<<- inverse
  getinverse <- function() inv
  list = list(get = get, set = set, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function takes the inverse of the matrix produced by the makeCacheMatrix
# function, checks if the inverse has been computed, if not computes it, otherwise
# returns the cached inverse

cacheSolve <- function(x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
