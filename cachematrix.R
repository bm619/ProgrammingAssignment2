## These functions will be used to cache the inverse of a matrix
## The First function creates a matrix object that can cache its inverse
## The Second function retrieves the calculated inverse of the matrix created in the first function from the cache or
## calculates the inverse of the matrix created in the first function

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function either retrieves the inverse of the above matrix (if matrix has not changed) from the cache or
## calculates the inverse if not alteady in cache or the matrix has changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
