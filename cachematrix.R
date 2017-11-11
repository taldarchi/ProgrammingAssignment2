## Assignment 2: Caching the Inverse of a Matrix

## creates a special "matrix" object that can cache its inverse.
## we can use it to get and set a matrix easily.

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## if the inverse is already computed the result stored inverse
## will be returned, otherwise it will be computed using the solve function and returned.

cacheSolve <- function(x, ...){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  ##set the new inverse for the special "matrix" object and also return it.
  x$setinverse(i)
  i
}