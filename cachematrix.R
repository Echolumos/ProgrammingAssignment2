## These functions compute the inverse matrix and store it
## or if it was computed before - return it from the cache

##  This function creates a special object
## that can cache matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  getinverse <- function() inverse
  setinverse <- function(inv) inverse <<- inv
  list(set = set, get = get, 
       getinverse = getinverse, 
       setinverse = setinverse)
}


## This function tests if the matrix is invertible and
## computes the inverse of the matrix and store it 
## via makeCacheMatrix function or return the computed one

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cashed data")
    return(inverse)
  }
  data <- x$get()
  if(nrow(data) != ncol(data)) {
    message('not invertible')
    return(NULL)
  }
  if(try(det(data)==0)) {
    message('not invertible')
    return(NULL)
  }
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
