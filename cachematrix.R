## Functions used to read and set a matrix and calcualtes its inverse
## by using the <<- operator which sets variable outside its environment

## function to get and set a matrix and its inverse

makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve reads an invertible function and check it its inverse already
## existing in cache, if so it caches the last value of the inverse
## if not, it calculates the inverse of the matrix

cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data :) ")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}