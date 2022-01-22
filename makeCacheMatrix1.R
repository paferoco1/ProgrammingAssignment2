setwd("C:/Users/Pablo/Documents/Assignment-2-Lexical-Scoping")
rm(list = ls())
cat("\014")

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


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

D <- matrix(c(4,5,6,7),2,2)
D1 <- makeCacheMatrix(D)
cacheSolve(D1)
