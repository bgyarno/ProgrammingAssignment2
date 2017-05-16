## These functions calculate the inverse of a matrix and cache the result.  
## Each time the functions are run, they check to see if the inverse has been 
## cached, if so they will return the inverse from the cache, if not an inverse 
## will be calculated and stored in the cache

## makeCacheMatrix function defines functions to set and get both the matrix 
## defined in the function argument and the inverse of the matrix, and returns 
## a list containing the functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## cacheSolve function uses the list of functions defined in makeCacheMatrix to 
## check if the inverse of the matrix already exists in the cache.  If it does, 
## it returns the cached value, if it does not it calculates the inverse.  
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
