#A set of functions that can cache the inverse of a matrix.

# THis function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) j <<- solve
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## This function
##computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the result is in the cache it retrieves it else it computes it.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
  
}

