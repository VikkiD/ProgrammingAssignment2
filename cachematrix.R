## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function creates a special matrix,
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ## compute the equality of the matrices
  matequal <- function(x, y)
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
  inverse <- NULL

  set <- function(y) {
    ## check equality before update.
    if (!matequal(x, y)) {
      x <<- y
      inverse <<- NULL
    }
  }

  get <- function() {
    ## return the given matrix
    x
  }

  setinverse <- function(y) {
    ## set the inverse
    inverse <<- y
  }

  getinverse <- function() {
    ## return the inverse
    inverse
  }

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a matrix.
## If we have already computed the inverse, return the result. Else recompute.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
