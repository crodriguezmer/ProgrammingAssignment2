## This function is like makeVector. It makes a matrix with a list of functions 
## to 1) set the value of the matrix, 2) get the value of the matrix, 3) set the
## value of the inverse, and 4) get the value of the inverse. The cool thing
## about it is that it does some cache magic.

makeCacheMatrix <- function(x = matrix()) {
  
  # Default the inverse value to NULL
  inv <- NULL
  
  # Cache the matrix and its default NULL inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Cache a non-NULL value as inverse. cacheSolve likes this function. 
  setinv <- function(inverse) inv <<- inverse
  
  # cacheSolve also likes this function. You may like it too.
  getinv <- function() inv
  
  # Print out the list of functions inside makeCacheMatrix. 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function is like cachemean. It uses the functions in makeCacheMatrix
## to check if the inverse of a matrix is already cached and computes it if
## isn't.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x',
  # where 'x' is the type of matrix object returned by makeCacheMatrix.
  
  # If the inverse is already calculated, just return it.
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If it hasn't been calculated, calculate it and return it.
  data <- x$get()         # get the matrix
  inv <- solve(data, ...) # compute its inverse (assuming data is invertible)
  x$setinv(inv)           # cache the new inverse
  return(inv)             # return it  
}
