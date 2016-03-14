## makeCacheMatrix creates a special matrix
## that knows how to cache its own inverse

## cacheSolve gets or calculates the inverse of the 
## special matrix object created via makeCacheMatrix


## makeCacheMatrix
## ====================================================
## input: invertible matrix

## returns special matrix object that supports 4 routines
##   - set 
##   - get (called by cacheSolve)
##   - setinverse (called by cacheSolve)
##   - getinverse (called by cacheSolve)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, 
        get = get,
        setinverse = setinverse, 
        getinverse = getinverse)
}


## makeCacheMatrix
## ====================================================
## input: special matrix object created via makeCacheMatrix

## returns inverse of special matrix

## inverse is retrieved using getinverse routine of 
## special matrix object if it is not NULL
## Otherwise, inverse is calculated in this function
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
