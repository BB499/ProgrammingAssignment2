#Matrix inversion is usually a costly computation 
#and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly.


#This function makes a matrix that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
     m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
        getinverse = getinverse)
  }
  

# This function returns the inverse of a matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
