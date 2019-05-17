## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

library(matlib)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invM <<- inverse
  getinverse <- function() invM 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- Inverse(data, ...)
  x$setinverse(invM)
  invM
}

## example

matrix = makeCacheMatrix(x= matrix(c(1,2,3,4),nrow=2,byrow = TRUE))
cacheSolve(matrix)


