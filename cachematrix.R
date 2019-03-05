## This two-part function gives the "Inverse of a Matrix"


## First fucntion "makeCacheMatrix" :
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) p <<- solve
  getinverse <- function() p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## First fucntion "cacheSolve" :
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}