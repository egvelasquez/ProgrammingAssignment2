## Put comments here that give an overall description of what your
## functions do

## This function is a list of functions that takes that takes a matrix as an argument
# and stores caches the inverse.

makeCacheMatrix <- function(x = as.matrix()){
  inv <- NULL
  set <- function(){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) inv<<- inverse
  get.inverse <- function() inv
  list = list(get = get, set = set, 
              set.inverse = set.inverse,
              get.inverse = get.inverse)
}


## This function takes the inverse of the matrix produced by the makeCacheMatrix
# function, checks if the inverse has been computed, if not computes it, otherwise
# returns the cached inverse

cacheSolve <- function(x,...){
  inv <- x$get.inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$set.inverse(inv)
  inv
}
