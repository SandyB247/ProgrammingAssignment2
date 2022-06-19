##Assignment 2


## Function - MakeCacheMatrix - Create a special matrix object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y) {
    x <<- y
    b <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) b <<- inverse
  getinverse <- function() b
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function - cacheSolve - Compute inverse of special matrix returned by 
## makeCacheMatrix
## If inverse already calculated (and matrix not changed), then
## cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  b <- x$getinverse()
  if (!is.null(b)) {
    message("getting cached data")
    return(b)
  }
  data <- x$get()
  b <- solve(data, ...)
  x$setinverse(b)
  b
}
