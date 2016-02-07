## Create a CacheMatrix object that takes a matrix and caches it's 
##    inverse in memory to avoid unneccessary recalculations

## makeCacheMatrix accepts a basic matrix and returns a CacheMatrix object
##    that keeps the basic matrix and it's inverse in closures

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   set <- function(newmatrix) {
      x <<- newmatrix
      inverse <<- NULL
      invisible(x)
   }
   get <- function() x
   setinverse <- function(inv) inverse <<- inv
   getinverse <- function() inverse
   list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
   )
}


## cacheSolve accepts a CacheMatrix object and returns the inverse.
##    returning the cached inverse if available, otherwise calculates
##    the inverse, sets the inverse in the CacheMatrix object and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   print('args', ...)
   inverse <- x$getinverse()
   if (!is.null(inverse)) {
      message("Inverse is Cached")
      return(inverse)
   } else {
      inverse <- solve(x$get(), ...)
      x$setinverse(inverse)
   }
   inverse
}
