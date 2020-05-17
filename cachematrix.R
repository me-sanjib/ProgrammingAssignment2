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
   # obtain the value of the matrix
  get <- function() x
    # invert the matrix and store in cache
  setinverse <- function(inverse) i <<- inverse
  # get the inverted matrix from cache
  getinverse <- function() i
  # return the created functions to the working environment
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
  ## Searching for the inverse of the matrix stored in cache
      i <- x$getinverse()
  # if  inverted matrix  found in the cache, returns it or creates the  matrix in the environment

  if (!is.null(i)) {
          message("getting cached data")
          # displaythe  matrix 
           return(i)
  }
  # creation of  new matrix if not exist
  data <- x$get()
  # set and return  the calculted inverted matrix
  i <- solve(data, ...)
  # save the inverted matrix 
  x$setinverse(i)
  # final display of the inverted matrix
  i
}
