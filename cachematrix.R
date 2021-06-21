## These functions cache the inverse of a matrix

## makeCacheMatrix() creates an object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() {x}
      setinverse <- function(inverse) {i <<- inverse}
      getinverse <- function() {i}
      list(set = set, 
           get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}


## cacheSolve() computes the inverse from the matrix returned by
## makeCacheMatrix() or retrieves the inverse from the cache if already
## calculated

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
