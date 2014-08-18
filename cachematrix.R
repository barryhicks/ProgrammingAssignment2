##
## M = makeCacheMatrix(m)
##   create an instance of a matrix class given
##   input matrix m
##
## cacheSolve(M): given an instance of the matrix class created
## by makeCacheMatrix, return the inverse of its matrix data.
## The inverse is cached so that it is only re-computed when the
## data values change.

## Creates a matrix class with data values of the given
## matrix and its cached inverse, and get/set member functions.
## Use this function to create an instance of this class
makeCacheMatrix <- function(m = matrix()) {
  # mInv is the cached matrix inverse
  # in the local environment of makeCacheMatrix.
  # Due to lexical scoping, this environment is pointed to
  # by the member functions defined below.
  # Similary, the parameter m will be saved in the same
  # environment.
  mInv <- NULL # data (along with m itself)
  # member functions
  set <- function(mNew) { # set new value for m
    m <<- mNew
    mInv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) mInv <<- inverse
  getinverse <- function() mInv
  # return member functions (along with environment containing
  # m and mInv)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a on object of the matrix class, created by the
## makeCacheMatrix function defined above, return the matrix's
## inverse.
## This is really the same code as cachemean in the example,
## I've just renamed the variables to match the context here.
cacheSolve <- function(M, ...) {
  
  mInv <- M$getinverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  mData <- M$get()
  mInv <- solve(mData) # inverse of a square matrix
  M$setinverse(mInv)
  mInv
}
