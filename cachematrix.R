## these functions allow a user to save time on getting the inverse
## of a matrix by using a cache to return the inverse... 
## if the matrix has already been solved

## this first function takes a matrix input
## and outputs a list of functions to be used in cacheSolve
## submit in form "cacheSolve(makeCacheMatrix(matrix))"
makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##named functions
}
## This function tests so see if the cach is null
## if the cache is not null it returns the cache
## if the cache is not null it returns the cached value
cachesolve <- function(x, ...) {
  m <- x$getinverse() ##calls the inverse from makeCacheMatrix
  if(!is.null(m)) { ## checks to see if it is not null
    message("hold on, gettin the inverse")
    return(m) ## returns the matrix if it not null
  }
  data <- x$get() ## returns that matrix
  m <- solve(data, ...) ## solve the matrix if it is null
  x$setinverse(m) ## cache the inverse 
  m  ## call the inverse for displat
}
