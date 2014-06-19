## makeCacheMatrix creates a special "matrix" object that can cache 
##   its inverse. It accepts a matrix as input and sets up an env
##   for the cacheSolve command to calculate the inverse.
makeCacheMatrix <- function(x = matrix()) {
   i <- NULL   ## clear the variable
   set <- function(y) { ## inner function to set the matrix using new value
      x <<- y  ## assign the value
      i <<- NULL      
   }
   get <- function() x ## function to retreive the value from cache
   setInverse <- function(solve) i <<- solve ## save the inverse of the matrix
   getInverse <- function() i ## get the inverse of the matrix
   list(set = set, get = get, ## list to store the commands
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then the cachesolve will 
##   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   i <- x$getInverse() ## use function from w/in makeCacheMatrix to get inverse from cache
   if(!is.null(i)){ ## check to see if it already exists
      message("retreving cached data")
      return(i) ## return the cached data
   }
   data <- x$get() ## use function from w/in makeCacheMatrix to get data from cache
   i <- solve(x, ...) ## invert the matrix
   x$setInverse(i) ## use function from w/in makeCacheMatrix to set data into cache
   i  ## Return a matrix that is the inverse of 'x' 
}
