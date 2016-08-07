## Cashing the Inverse of a Matrix similar to caching mean of a vecotr
## Instead of computing it repeatedly, caching may be beneficial which will increase the computation speed
## 
## The following functions will create Matrix, compute inverse (retrieve input from cache if exists)- using lexical scoping
## First create a "matrix" (really a LIST) object 
## ASuthor: Viswanath Narayan

makeCacheMatrix <- function(x = matrix()) {
## assigning the Inverse Matrix inv  NULL value  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setImatrix <- function(solve) inv <<- solve
  getImatrix <- function() inv
  list(set = set,
       get = get,
       setImatrix = setImatrix,
       getImatrix = getImatrix
       )
  
}



##The following function computes the inverse of the special "matrix" created 
##with the makeCacheMatrix above. It first checks to see if the inverse  has already
##been computed and the matrix has not changed. If so, it gets the mean inverse of the matrix from cache and skips the computation.
##Otherwise, it computes  the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getImatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setImatrix(inv)
  inv
}
