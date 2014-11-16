## makeCacheMatrix creates special matrix object that can cache it's inverse

makeCacheMatrix <- function(x = numeric()) {
  # We'll put our inverse matrix to this variable
  m <- NULL

  # set function, cache it once you have it
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Inline function definition, returns x
  get <- function() x

  # Inverse matrix and add it to variable m with caching
  setsolve <- function(solve) m <<- solve

  # Get it
  getsolve <- function() m

  # Coerce and check lists
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculates inverse of matrix created with makeCacheMatrix. If inverse is already calculated, return it without
## calculating it again

cacheSolve <- function(x, ...) {
  # Get inverse matrix from makeCacheMatrix, save it to variable s
  s <- x$getsolve()

  # If it's cached, return it, don't inverse it again
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

  # ...but if not, inverse it and cache it, and return it back.
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## To test it:
## Create 2x2 matrix and assign it to the variable mtrx (having values 1, 2, 3, 4) with:
## mtrx <- matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
##
## Cache inverse matrix of mtrx, and add it to variable, we'll call it inv:
## inv <- makeCacheMatrix(mtrx)
##
## Test it with:
## cacheSolve(inv)
