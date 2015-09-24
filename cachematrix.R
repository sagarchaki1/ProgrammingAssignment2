## The two functions below cache the inverse of a matrix.
## Assuming it is always a square invertible matrix


## Function 1
## The makeCacheMatrix function creates a "matrix" object that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setmean = setinv,
       getmean = getinv)
}


## Function 2
##  the function computes the inverse of the "matrix" returned by makeCacheMatrix function. 
## If the inverse has already  been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}




