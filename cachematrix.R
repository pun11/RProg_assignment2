## Coursera R Programming Assignment 2
## Calculating an inverse utilizing cached values

## Create a cache of a matrix for easier and faster computations

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) M <<- solve
  getinv <- function() M
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Solving an inverse of a given matrix. If it was already computed and cached, returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  M <- x$getinv()
  if (!is.null(M)) {
    message("Getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinv(M)
  M
}