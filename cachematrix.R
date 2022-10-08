## Isaias Mendes Machado
## My functions do the following: cache a matrix to afterwards used it in more
## fast way to get your inverse

## This function returns an object with "subfunctions" from a matrix passed as 
## argument, doing so, you have a cache to be used once that function have 
## already been executed. If you want change the original matrix you can use the 
## set function. After that, you need new running to cache the matrix again.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function receive the matrix created by the makeCacheMatrix, it checks if 
## the inverse one exist. If so, just return it. Else, calculates the matrix
## that is  inverse one.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
