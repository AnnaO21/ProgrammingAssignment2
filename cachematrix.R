## My functions get the inverse of a matrix, 
## but when the inverse has been calculated once it will remember the outcome.
## Every time you try to calculate the inverse again it will just return what it remembered.

## This function makes a 'special matrix' of the matrix you give to the function.
## It returns a list with the matrix, a value for i and four functions which can be used
## by calling them with makeCacheMatrix(m)$
## The function set sets the value of y to x in the global environment
## The function get gets the value of x
## The function setinverse sets the value of inverse to i in the global environment
## The function getinverse gets the value of i


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


## This functions calculates the inverse of the 'special matrix' x if it is unknown.
## When it has been calculated before it returns the inverse.
## First it gets the value i from x, if this is NULL (this is the first time you call this function)
## it gets the value of the matrix from the 'special matrix' x and calculated it inverse.
## It then sets the value of the inverse to the global environment.
## The next time you call cacheSolve it will find the inverse and it will print "getting cached data"

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
