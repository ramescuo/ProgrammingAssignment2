## This pair of functions are used to compute a matrix inversion or return 
## a cached inversion if it already exists

## This function creates the special matrix with a place to store the inversion

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x', it computes 
## and stores the value if it dosn't exists or retruns the stored value

cacheSolve <- function(x, ...) {
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
