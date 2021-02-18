## Writing a pair of functions that cache the inverse
## of a matrix

## this function will create a "matrix" object than 
## can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x} 
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {inv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will "search" to see if the inverse
##of the "matrix" has been calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matr <- x$get
  inv <- solve(matr,...)
  x$setinv(inv)
  inv
}
