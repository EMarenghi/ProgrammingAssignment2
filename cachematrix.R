## This function creates a matrix object that can cache its inverse.
## This function is useful because it allows the user to return the
## cached value of an inverse matrix rather than computing it repeatedly. 

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
              x <<- y
              inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get, setinverse = setinverse,
                 getinverse = getinverse)
}


## This function computes the inverse of the matrix created in the 
## function above. If the inverse matrix has already been calculated,
## cacheSolve returns the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
      
}
