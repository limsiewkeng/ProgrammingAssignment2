## makeCacheMatrix creates our own data type which contains original matrix
## and its inverse matrix in a list

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
      inv <<- inverse
  }
  
  getInverse <- function() {
    inv
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve tries to get inverse matrix from our cacheMatrix data type, if it finds
## it returns directly from cache, else it solves inverse and stores in cache and returns result

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  
  inv
}
