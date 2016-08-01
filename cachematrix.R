## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix <- function(x = matrix()) {

## }
makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setImatrix <- function(solve) n <<- solve
    getImatrix <- function() n
    
    list(set = set, get = get,
         setImatrix = setImatrix,
         getImatrix = getImatrix)

}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.Write a short comment describing this function


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

}
