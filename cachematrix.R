## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions that sets and gets the value of the list and sets and gets the value of the 
## inverted matrix

makeCacheMatrix <- function(x = matrix()) {
# Constructs an object 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # sets the Cache value
  setCache <- function(invert) m <<- invert
  # returns the Cached value
  getCache <- function() m
  # returns a list
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 
 # assigns m to Cached value
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if Cached value is null, calculates the inverted 
  data <- x$get()
  m <- solve(data)
  x$setCache(m)
  m
}

