setwd("C:/Users/Pablo/Documents/Assignment-2-Lexical-Scoping")
rm(list = ls())
cat("\014")

## There are two functions makeCacheMatrix and cachesolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ## This Function gets Matrix X
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function gets the cache data

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  i
}

## This is a test of the code and its result
D <- matrix(c(4,5,6,7),2,2)
D1 <- makeCacheMatrix(D)
cacheSolve(D1)
