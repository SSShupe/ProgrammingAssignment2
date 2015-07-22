## Utilizes R's lexical scoping rules to create a matrix and
## an alternative "Solve()" function. Together these allow for the
## computation of the inverse of the created matrix to be cached
## so that the value need not be recomputed by R each time it is needed. 

## The following code creates a function that creates an object to
## create, store, and retrieve a matrix and the value of the inverse
## of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) m <<- solve
    get_inverse <- function() m
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## The function below calculates the inverse of the matrix created by
## the function above. If the inverse has been calculated previously, the
## function retrieves it from the cache; if not, it calculates the
## inverse using the "Solve()" function and then caches it.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$set_inverse(m)
    m
}
