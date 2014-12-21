## These two methods provide for caching the inverse of a matrx.  A cache matrix is created
## with the makeCacheMatrix function.  To solve for the inverse use the cacheSolve function.

## Create a special matrix that caches it's inverse.  Base matrix is accessed
## with get and set functions.  The inverse is set or retrived with setInverse
## and getInverse functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of the special matrix.  If a cached version of the inverse
## is availabe, return it.  If the cache is empty then calculate the inverse
## and cache it for future requests.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()
     if (!is.null(inv)) {
       message("getting cached inverse")
       return (inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setInverse(inv)
     inv
}
