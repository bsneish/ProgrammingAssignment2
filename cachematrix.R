## Caching the inverse of a matrix, as it is a costly computation


## creates a special "matrix", which is really a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Used with makeCacheMatrix. Checks if inverse exists in cache and if so retrieves. Else solve inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
