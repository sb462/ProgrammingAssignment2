## Put comments here that give an overall description of what your
## functions do

## To invert a matrix, set the matrix as part of the object makeCacheMatrix using the set function
## Then call the cacheSolve function with the argument being the object constructed by makeCacheMatrix, 
## will print out the inverted matrix.

## makeCacheMatrix creates a list that 
## sets and gets the value of the Cache

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
  # returns the Cached value for m
  getCache <- function() m
  # returns a list
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## Write a short comment describing this function
## Returns the cached value. If inverse is already calculated and cached, returns cache, otheriwse calculates inverse of
## the matrix passed to the makeCacheMatrix function and prints it


cacheSolve <- function(x, ...) {
 
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

## below is an example of application of the above
mat <- matrix(c(1,2,4,5,6,7,8,1,2), nrow=3,ncol=3)
go <- makeCacheMatrix()
go$set(mat)
go$get()
cacheSolve(go)
