## To find the inversion  of a matrix is very time consuming phenomenon. 
##It is  always batter  to caching the inverse of a matrix despite of compute it when required. 
##The following function  will be used to create a to caches its inverse.
######################################################################################
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
########################################################################################################
## The following code is used to compute  the inverse of the  "matrix" generated   by the function 
##makeCacheMatrix as  above. This code will find whether  the inverse of the same  matrix has already 
##been calculated or not , if so then  then it w will obtain the inverse result from the cache.
#############################################################################################################
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
