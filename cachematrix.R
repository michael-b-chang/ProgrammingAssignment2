## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y) {
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  setrev <- function(s) rev <<- s
  getrev <- function() rev
  list(set = set, get = get,
       setrev = setrev,
       getrev = getrev)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getrev()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setrev(s)
  s
}
