##  Note: <<- operator - can be used to assign a value to an object in an environment
## that is different from the current environment
## 
## Functions to create Matrix, compute inverse (retrieve input from cache if exists)- using lexical scoping
## First create a "matrix" (really a LIST) object 

## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setImatrix <- function(solve) m <<- solve
  getImatrix <- function() m
  
  list(set = set, get = get,
       setImatrix = setImatrix,
       getImatrix = getImatrix)
  
}


## Write a short comment describing this function
##The following function calculates the mean of the special "vector" created 
##with the above function. However, it first checks to see if the mean has already
##been calculated. If so, it gets the mean from the cache and skips the computation.
##Otherwise, it calculates the mean of the data and sets the value of the mean in the 
##cache via the setmean function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getImatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setImatrix(m)
  m
}

