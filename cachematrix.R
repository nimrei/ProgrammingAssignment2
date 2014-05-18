# The following functions are used to cache the
# computation of a matrix inverse

# makeCacheMatrix
# inputs: 
#     x - matrix (defaults to 1x1 matrix [1])
# outputs: 
#     a special cached matrix 
#     i.e. a list of utlity functions based on the input matrix
#     and calculating its inverse
#       set() - sets the value of the matrix
#       get() - gets the value of the matrix
#       setinverse() - sets the inverse of the matrix 
#       getinverse() - gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y

    # because we've changed x, clear the cached inverse 
    # so we know when to recalculate it
    x_inverse <<- NULL 
  }
  get <- function() x
  setInverse <- function(inv) x_inverse <<- inv
  getInverse <- function() x_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# cacheSolve
# inputs: 
#     x - cached matrix with list of utlity functions 
#         based on the input matrix (see output of makeCacheMatrix)
# outputs:
#     The cached inverse of the input cachedMatrix
cacheSolve <- function(x, ...) {
  x_inv <- x$getInverse()

    # check if we've calculated the inverse already (i.e. its cached)
  if(!is.null(x_inv)) { 
    message("getting cached data")
    return(x_inv)
  }

    # otherwise recalculate the inverse based on x's current data
  data <- x$get()
  x_inv <- solve(data, ...)

    # store it in the cache
  x$setInverse(x_inv)

    # return the inverse
  x_inv 
}
