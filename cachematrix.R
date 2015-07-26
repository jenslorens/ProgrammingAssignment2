makeCacheMatrix <- function(x = numeric()) {
  
  # Initiate the local storage to NULL
  
  CacheMatrix <- NULL
  
  # Assign value to main argumnet
  setMatrix <- function(Incoming) {
    x <<- Incoming
    CacheMatrix <<- NULL
  }
  
  # Get the value from the argument
  getMatrix <- function() {
    x
  }
 
  # cache the inversed argument
  cacheInverse <- function(solve) {
    CacheMatrix <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    CacheMatrix
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

cacheSolve <- function (y, ...) {
  # Obtain the cached value
  Result <- y$getInverse()
  # Check if the value exists, then return it
  if(!is.null(Result)) {
    message("data was cached")
    return(Result)
  }
  # If first time, calculate the inverse and store in cache
  else {
    data <- y$getMatrix()
    Result <- solve(data)
    y$cacheInverse(Result)
  }
  
  Result
  
}

