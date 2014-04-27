## The following 2 functions are used to compute the inverse of matrix
## However, if the inverse has been computed the function will return the cashed inverse 
## This is done to save time as matrix inversion is an expensive procedure

## The function makeCacheMatrix creates a list of the functions to set and get the matrix.
## As well as setting the inverse and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse    # assigning the inverse to m
  getInverse <- function() m                       # getting the inverse if available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cachesolve gets the cashed inverse of a matrix if available, or else computes it

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()             # Getting the matrix we want to compute the inverse of
  m <- solve(data, ...)       # Finding the inverse
  x$setInverse(m)             # Cashing the inverse to be used later
  m
  
}

#This conclude the code