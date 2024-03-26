## Put comments here that give an overall description of what your
## functions do

## A function to create a 'special' matrix which supports
## chached inversion, supports four functions: get, set,
##  setinv (solves the matrix), and getinv (returns the inverted matrix)

makeCacheMatrix <- function(x = matrix()) {
  # Set the variable containing the inverse to NULL
  m <- NULL
  # Set function. Ses a provided matrix to be the stored matrix to be solved
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Get function. Returns the matrix to be solved.
  get <- function() x

  # setinv function solves the matrix and storied the result in 'm'
  setinv <- function(solve) m <<- solve

  # getinv function returns the inverted matrix stored in 'm'
  getinv <- function() m

  # returns a list of the functions available
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## An adapted function to solve matrices. Input is a 'special' matrix created
## with 'makeCacheMatrix' above. It returns the solution to the matrix stored in
## the cachematrix - either by returning the cached solution, or ... if no cache
## solution exists, by solving it for the first time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the solution from the cachematrix provided
  m <- x$getinv()
  # if there is a solution return it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherrwise get the matrix, solve it, and put the result into the solution
  # variable
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  # finally return the solution
  m

}
