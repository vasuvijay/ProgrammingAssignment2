# Big Picture
# The idea is to cache potentially time-consuming computations.
# Finding the inverse of a matrix is a time consuming operation,
# especially if large matices are involved and the operation
# has to be done repeatedly (e.g. in a loop). If the contents
# of the matrix are not changing, it may make sense to cache
# the value of the inverse so that when we need it again, it
# can be looked up in the cache rather than recomputed. 

# Function One
# This function creates a special "matrix" object that 
# can cache its inverse.
# Input is a matrix for which the function calculates the inverse
# and saves/caches it. It can be called to get or set the
# initial matrix and get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL # initialize m
  
  # Set x to be y
  set <- function(y) {
    x <<- y     # assignment is in the global env
    m <<- NULL  # assignment is in the global env
  }
  
  # return x to the caller
  get <- function() x 
  
  # Get the inverse of the matrix using solve and
  # set m to the inverse
  setinverse <- function(solve) m <<- solve
  
  # return m (which is the inverse) to the caller
  getinverse <- function() m
  
  # set of commands/ananymous function in this function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# Function Two
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated previously (and the matrix has not
# changed), then cachesolve retrieves the inverse from 
# the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # See if the inverse is already computed by asking
  # for it from the makeCacheMatrix function
  m <- x$getinverse()
  
  # If we did get the cached inverse, then return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if we did not find the inverse in the cache, then we
  # find the inverse and save it in the cache
  data <- x$get()     # get the matrix x
  m <- solve(data)    # get its inverse
  x$setinverse(m)     # save it in the cache
  m                   # return the inverse   
  
}
