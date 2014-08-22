## Here we have two functions : makeCacheMatrix ,and cacheSolve
## makeCachMatrix is somehow a wrapper for R matrix ,with the ability to cache and store some values (the inverse in this example)
## cacheSolve will calculate the inverse,but first it will access the cache in CacheMatrix, if its not null ,so it will calculate it and store it inside CacheMatrix

## This function will create the cache-matrix

makeCacheMatrix <- function(x = matrix()) {
  #i is the inverse
  i <- NULL
  
  #set the matrix and reset the inverse to be NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #return the Matrix
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will create inverse and set it if it is null
## Otherwise it will get it from Cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse ...")
    return(i)
  }
  message("calculating the inverse ...")
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
