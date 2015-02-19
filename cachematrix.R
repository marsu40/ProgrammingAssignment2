## Set of functions allowing to cache the inverse of a matrix
## 

## Repository object containing the matrix 
## and posisble inverse if already calculated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function calculating the inverse of the matrix
## if it was already calculated the cached value will be used
## if not the value will be cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
