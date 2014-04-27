## These functions create a special object that stores a square matrix and caches the inverse of that matrix.
## 

## This function creates square matrix in order to set and get the matrix and then set and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
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


## This function calculates the invers of the special matrix in the first function.

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i)){
          message("getting cached data")
          return(i)
    }
    data<-x$get()
    i<-inverse(data, ...)
    x$setinverse(i)
    i
}
