## Programming Assignment 2
## This is a pair of functions that can cache the inverse of a matrix

## This funciton creates a special matrix object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Set m to NULL initially to indicate the inverse is not cached
  m <- NULL ## When m is null, the inverse has not been cached
  set <- function(y) {
    x <<- y ## The "set" function assigns a new value to the matrix
    m <<- NULL ## inverse of this new matrix hasn't been cached
  }
  ## The get function returns the user-defined matrix, x
  get <- function() x
  
  ## The setinv function puts the inverse of the matrix in the cache
  setinv <- function(solve) m <<- solve
  
  ## The getinv function returns NULL if the inverse isn't cached yet
  ## and returns the value of the inverse if it is cached
  getinv <- function() m
  
  ## The four functions defined here
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special matrix object
## that was created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## Calls getinv, returns NULL if inverse isn't cached
  m <- x$getinv()
  if(!is.null(m)) {
    ## getinv didn't return NULL, so it returned the cached inverse
    message("getting cached data")
    return(m)
  }
  
  ## The inverse isn't cached, so compute it
  ## First, get the value of the user-defined matrix
  thematrix <- x$get()
  
  ## Then calculate its inverse
  m <- solve(thematrix)
  
  ## Then cache the inverted matrix
  x$setinv(m)
  
  ## And return the cached inverse
  m
}