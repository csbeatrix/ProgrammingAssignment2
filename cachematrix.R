## I created 2 functions named makeCacheMatrix and cacheSolve
## The first "creates a special "matrix" object that can cache its inverse"
## The second either computes the inverse of a matrix returned by makeCacheMatrix,
## or retrieves the value from the cache (if it was already calculated).

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
      x <<- y
      inver <<- NULL
  }
  get <- function() x
  setInverse <-function(inverse) inver <<- inverse
  getInverse <-function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
      message("Getting data from cache")
      return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setInverse(inver)
  inver
}
