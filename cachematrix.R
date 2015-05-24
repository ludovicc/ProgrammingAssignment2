## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.
## makeCacheMatrix and cacheSolve can be used to create a matrix that is able to cache its inverse.
## Typical usage:
##     ````
##     m <- makeCacheMatrix(matrix(1:4,2,2))
##     cacheSolve(m)
##     m$get()
##     cacheSolve(m)
##     ````

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse metrix
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## cacheSolve takes a cached matrix as argument and returns the inverse of the matrix.
## The inverse is stored inside the cached matrix on first call, then retrived from the cache on subsequent calls.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
