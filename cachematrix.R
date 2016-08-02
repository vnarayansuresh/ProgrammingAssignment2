## ## Note: <<- operator - can be used to assign a value to an object in an environment
## that is different from the current environment
## 
## Functions to create Matrix, compute inverse (retrieve input from cache if exists)- using lexical scoping
## First create a "matrix" (really a LIST) object 

makeCacheMatrix <- function(x = matrix()) {
    inv1 <- NULL
    set <- function(y) {
      x <<- y
      inv1 <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv1 <<- inverse
    getinverse <- function() inv1
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Now solve the inverse - use cached input data if exists 

#cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    inv1 <- x$getinverse()
    if(!is.null(inv1)) {
      message("getting cached data")
      return(inv1)
    }
    matrixdata <- x$get()
    inv1 <- solve(matrixdata, ...)
    x$setinverse(inv1)
    inv1
}
