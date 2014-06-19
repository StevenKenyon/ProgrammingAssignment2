## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache 
##   its inverse. It accepts a matrix as input and sets up an env
##   for the cacheSolve command to calculate the inverse.
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
      x <<- y
      i <<- NULL      
   }
   get <- function() x
   setInverse <- function(solve) i <<- solve ## save the inverse of the matrix
   getInverse <- function() i ## get the inverse of the matrix
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then the cachesolve will 
##   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   solve(x, ...)
}
