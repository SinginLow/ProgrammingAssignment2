## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     # Initializes m
     m <- NULL
     
     # creates function object to return value of original matrix
     get <- function() x
     
     # creates function object to store calculated inverse matrix
     setinverse <- function(q) m <<- q
     
     # creates function object to return stored inverse
     getinverse <- function() m
     
     # returns list containing function objects created above
     list(get = get, setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
     # assigns variable m the value stored in x$getinverse
     m <- x$getinverse()
     
     # check for cached value
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # assigns original matrix value to data
     data <- x$get()
     
     # calculates inverse of original matrix
     m <- solve(data, ...)
     
     # stores the inverse value
     x$setinverse(m)
     
     # returns m
     m
}
