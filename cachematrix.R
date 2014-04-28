## This is a pair of functions which checks whether a cache contains
## the inverse of matrix, and if not, will calculate it

## Creates a list of getters/setters associated which can set/get the
## value of the matrix and set/get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## See if the inverse has been cached. If so, return it.
## Otherwise, calculate the inverse and store it for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

# Sample test code
# mat1 <- matrix(c(4,2,7,6), 2)
# cmat1 <- makeCacheMatrix(mat1)
# cacheSolve(cmat1)
