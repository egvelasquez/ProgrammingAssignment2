## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
