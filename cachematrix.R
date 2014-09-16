## These two functions will allow you to enter a matrix and cache its inverse,
## allowing you to save computation time in the future when recalling the inverse

## Creates a list of functions that will find a matrix, calculate its inverse and return
## both the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## checks to see if inverse of matrix has been cached
## if yes, returns the cached inverse
## if no, calculates the inverse of the matrix and returns it 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}