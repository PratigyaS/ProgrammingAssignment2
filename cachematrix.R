## The function caches the matrix inverse value and returns the inverse of the 
##  provided by the user

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #This is the inverse of the matrix set as NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


#Checks if the inverse is already in cache if yes it will show the cached inverse
#else creates inverse saves it in cache and shows the outp

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if (!is.null(i)){
    
    message("getting cached data")
    return(i)
    
  }
  
  # this function calculates the inverse of the given matrix
  data <- x$get()
  i <- solve(data, ...)
   x$setinverse(i)  
  
  #returns the inverse value
  return(i)
  
  
}

